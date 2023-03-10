#pragma once

#include "lzhcc.h"

namespace lzhcc {

class Variable {
  enum class Kind : uint8_t {
    null,
    value,
    type,
    cint,
  };
  using enum Kind;

public:
  Variable() : data(0), kind(null) {}
  Variable(Value *value) : data(from(value)), kind(Kind::value) {}
  Variable(Type *type) : data(from(type)), kind(Kind::type) {}
  Variable(int value) : data(value), kind(Kind::cint) {}
  operator Value *() { return kind == value ? into<Value>() : nullptr; }
  operator Type *() { return kind == type ? into<Type>() : nullptr; }
  auto get(int *value) -> bool { return *value = data, kind == cint; }

private:
  static auto from(void *pointer) -> uint64_t {
    return reinterpret_cast<uint64_t>(pointer) >> 2;
  }
  template <class T> auto into() -> T * {
    return reinterpret_cast<T *>(data << 2);
  }
  uint64_t data : 62;
  Kind kind : 2;
};

struct Scope {
  int old_stack_size;
  std::unordered_map<int, Variable> var_map;
  std::unordered_map<int, Type *> tag_map;
};

using ParamNames = std::vector<Token *>;

auto low_refernce_op(Context *, Expr *) -> Expr *;
auto low_deref_op(Context *, Expr *, int) -> Expr *;
auto low_cast_op(Context *, Type *, Expr *) -> Expr *;
auto convert(Context *, Expr *, Expr *, int)
    -> std::tuple<Expr *, Expr *, Type *>;
auto convert_cmp(Context *, Expr *, Expr *, int) -> std::pair<Expr *, Expr *>;
auto convert(Context *, Expr *, int) -> std::pair<Expr *, Type *>;
auto low_mul_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_div_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_mod_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_add_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_sub_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_shift_left_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_shift_right_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_bitwise_and_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_bitwise_xor_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_bitwise_or_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_assign_op(Context *, Expr *, Expr *, int) -> Expr *;
auto low_member_op(Context *, Expr *, int, int) -> Expr *;

class Parser {
public:
  Parser(Token *position, Context *context)
      : position_(position), context_(context) {}
  auto operator()() -> Module;
  auto const_int(int64_t *value) -> bool;

private:
  auto scalar_init() -> Init *;
  auto array_init(ArrayType *array) -> Init *;
  auto record_init(RecordType *record) -> Init *;
  auto init(Type *type) -> Init *;

  auto array_designator() -> int;
  auto array_designation(Type* type, Init *&init) -> void;

  auto array_init(ArrayType *array, ArrayInit *&init, int i = 0) -> void;
  auto record_init(RecordType *record, RecordInit *&init) -> void;
  auto init(Type *type, Init *&init) -> void;

  auto low_local_scalar(Expr *expr, ScalarInit *init, int loc) -> Expr *;
  auto low_local_array(Expr *expr, ArrayInit *init, int loc) -> Expr *;
  auto low_local_record(Expr *expr, RecordInit *init, int loc) -> Expr *;
  auto low_local(Expr *value, Init *init, int loc) -> Expr *;

  auto low_global_scalar(ScalarInit *init, std::span<uint8_t> out,
                         std::vector<Relocation> &relocations, int loc) -> void;
  auto low_global_array(ArrayInit *init, std::span<uint8_t> out,
                        std::vector<Relocation> &relocations, int loc) -> void;
  auto low_global_record(RecordInit *init, std::span<uint8_t> out,
                         std::vector<Relocation> &relocations, int loc) -> void;
  auto low_global(Init *init, std::span<uint8_t> out,
                  std::vector<Relocation> &relocations, int loc) -> void;

  auto numeric() -> Expr *;
  auto integer(Token *token) -> Expr *;
  auto floating(Token *token) -> Expr *;
  auto cook_string(IntegerType *&type) -> std::string;
  auto string() -> Expr *;
  auto character() -> Expr *;
  auto call(Expr *func, FunctionType *type) -> Expr *;
  auto primary() -> Expr *;
  auto unary() -> Expr *;
  auto cast() -> Expr *;
  auto post_inc(Expr *lhs, Expr *rhs, int loc) -> Expr *;
  auto postfix() -> Expr *;
  auto multiplicative() -> Expr *;
  auto additive() -> Expr *;
  auto shift() -> Expr *;
  auto relational() -> Expr *;
  auto equality() -> Expr *;
  auto bitwise_and() -> Expr *;
  auto bitwise_xor() -> Expr *;
  auto bitwise_or() -> Expr *;
  auto logical_and() -> Expr *;
  auto logical_or() -> Expr *;
  auto condition() -> Expr *;
  auto assignment() -> Expr *;
  auto expression() -> Expr *;

  auto const_int(Expr *expr, int64_t *value, std::string_view **lable) -> bool;
  auto const_float(Expr *expr, double *value) -> bool;

  using LowFn = Expr *(Context *, Expr *, Expr *, int);
  auto assign_to(Expr *lhs, Expr *rhs, LowFn lower, int loc) -> Expr *;

  auto block_stmt() -> Stmt *;
  auto asm_stmt() -> Stmt *;
  auto expr_stmt() -> Stmt *;
  auto for_stmt() -> Stmt *;
  auto if_stmt() -> Stmt *;
  auto return_stmt() -> Stmt *;
  auto while_stmt() -> Stmt *;
  auto goto_stmt() -> Stmt *;
  auto label_stmt() -> Stmt *;
  auto switch_stmt() -> Stmt *;
  auto case_stmt() -> Stmt *;
  auto default_stmt() -> Stmt *;
  auto do_stmt() -> Stmt *;
  auto statement() -> Stmt *;

  auto enum_spec() -> Type *;
  auto struct_decl(RecordType *type) -> void;
  auto union_decl(RecordType *type) -> void;
  auto struct_or_union_decl() -> Type *;

  struct VarAttr {
    bool is_typedef;
    bool is_static;
    bool is_extern;
    bool is_inline;
    int align_bytes;
  };

  auto is_typename(Token *token) -> bool;
  auto declspec(VarAttr *attr = 0) -> Type *;
  auto pointers(Type *base) -> Type *;
  auto array_dimensions(Type *base) -> Type *;
  auto function_parameters(Type *base, ParamNames *param_names) -> Type *;
  auto suffix_type(Type *base, ParamNames *param_names) -> Type *;
  auto declarator(Type *base, ParamNames *param_names = 0)
      -> std::pair<Token *, Type *>;
  auto abstract_declarator(Type *base) -> Type *;
  auto type_define(Type *base) -> void;
  auto declaration() -> Stmt *;
  auto global(Token *name, Type *base, Type *type, VarAttr *attr) -> void;
  auto function(Token *name, Type *type, ParamNames param_names,
                Linkage linkage) -> void;

  auto next_is(TokenKind kind) -> bool;
  auto next_kind() -> TokenKind;
  auto consume() -> Token *;
  auto consume(TokenKind kind) -> Token *;
  auto consume_if(TokenKind kind) -> Token *;

  auto entry_scope() -> void;
  auto leave_scope() -> void;
  auto create_declaration(Token *token, Type *type) -> void;
  auto create_typedef(Token *token, Type *type) -> void;
  auto create_enum(Token *token, int value) -> void;
  auto create_local(Token *token, Type *type, int align) -> LValue *;
  auto create_local(std::string name, Type *type) -> LValue *;
  auto create_global(Token *token, Type *type, uint8_t *init,
                     std::vector<Relocation> relocations, int align_bytes,
                     Linkage linkage) -> void;
  auto create_function(Token *token, Type *type, int stack_size, Stmt *stmt,
                       std::vector<LValue *> params, LValue *va_area,
                       Linkage linkage) -> void;
  auto create_anon_global(Type *type, uint8_t *init, std::vector<Relocation> r)
      -> GValue *;
  auto create_anon_local(Type *type) -> LValue *;
  auto create_tag(Token *token, Type *type) -> void;
  auto get_or_create_tag(Token *token) -> RecordType *;
  auto find_var(int name) -> Variable;
  auto find_value(int name) -> Value *;
  auto find_type(int name) -> Type *;
  auto find_tag(int name) -> Type *;
  auto unique_name() -> std::pair<std::string_view, int>;

  Token *position_;
  Context *context_;

  Type *ret_;
  std::stack<Label *> breaks_;
  std::stack<Label *> continues_;
  std::stack<SwitchStmt *> switchs_;
  std::unordered_map<int, Label *> lable_map_;

  int stack_size_;
  int max_stack_size_;
  std::deque<Scope> scopes_;
  int unique_id_ = 0;
};

} // namespace lzhcc