#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::enum_spec() -> Type * {
  consume(TokenKind::kw_enum);
  auto name = consume_if(TokenKind::identifier);
  if (name && !next_is(TokenKind::open_brace)) {
    if (auto type = find_tag(name->inner)) {
      return type;
    } else {
      context_->fatal(name->location, "");
    }
  }
  consume(TokenKind::open_brace);
  int value = 0;
  bool is_first = true;
  while (!consume_if(TokenKind ::close_brace)) {
    if (!is_first) {
      consume(TokenKind::comma);
    }
    is_first = false;
    auto ident = consume(TokenKind::identifier);
    if (auto token = consume_if(TokenKind::equal)) {
      if (int64_t tmp; !const_int(&tmp)) {
        context_->fatal(token->location, "");
      } else {
        value = static_cast<int>(tmp);
      }
    }
    create_enum(ident, value++);
  }
  auto type = context_->int32();
  if (name) {
    create_tag(name, type);
  }
  return type;
}

auto Parser::struct_or_union_decl() -> Type * {
  void (Parser::*ptr)(RecordType *) = nullptr;
  if (consume_if(TokenKind::kw_struct)) {
    ptr = &Parser::struct_decl;
  } else if (consume_if(TokenKind::kw_union)) {
    ptr = &Parser::union_decl;
  }
  auto name = consume_if(TokenKind::identifier);
  RecordType *type = nullptr;

  //
  // case 1:
  // struct { int x; };
  //
  // case 2:
  // struct A { int x; };
  //
  // case 3:
  // struct A *ptr;
  // struct A { int x };
  //
  // case 4:
  // struct A { int x };
  // struct A a;
  //

  if (!name) {
    // case 1
    type = context_->record_type();
    (this->*ptr)(type);
  } else if (next_is(TokenKind::open_brace)) {
    // case 2
    type = get_or_create_tag(name);
    (this->*ptr)(type);
  } else {
    auto tag = find_tag(name->inner);
    // case 3 and case 4
    if (!tag || tag->kind != TypeKind::record) {
      type = context_->record_type();
      create_tag(name, type);
    } else {
      type = cast<RecordType>(tag);
    }
  }
  return type;
}

auto Parser::struct_decl(RecordType *type) -> void {
  consume(TokenKind::open_brace);
  int offset = 0;
  int align_bytes = 1;
  std::vector<Member> members;
  while (!consume_if(TokenKind::close_brace)) {
    auto base = declspec();
    bool is_first = true;
    while (!consume_if(TokenKind::semi)) {
      if (!is_first) {
        consume(TokenKind::comma);
      }
      is_first = false;
      auto [name, type] = declarator(base);
      int size = context_->size_of(type);
      int align = context_->align_of(type);
      offset = align_to(offset, align);
      members.push_back(Member{type, name->inner, offset});
      offset += size;
      align_bytes = std::max(align_bytes, align);
    }
  }
  int size_bytes = align_to(offset, align_bytes);
  type->members = std::move(members);
  type->size_bytes = size_bytes;
  type->align_bytes = align_bytes;
}

auto Parser::union_decl(RecordType *type) -> void {
  consume(TokenKind::open_brace);
  int size_bytes = 0;
  int align_bytes = 1;
  std::vector<Member> members;
  while (!consume_if(TokenKind::close_brace)) {
    auto base = declspec();
    bool is_first = true;
    while (!consume_if(TokenKind::semi)) {
      if (!is_first) {
        consume(TokenKind::comma);
      }
      is_first = false;
      auto [name, type] = declarator(base);
      int size = context_->size_of(type);
      int align = context_->align_of(type);
      members.push_back(Member{type, name->inner, 0});
      size_bytes = std::max(size_bytes, size);
      align_bytes = std::max(align_bytes, align);
    }
  }
  type->members = std::move(members);
  type->size_bytes = align_to(size_bytes, align_bytes);
  type->align_bytes = align_bytes;
}

enum {
  kw_void = 1 << 0,
  kw_bool = 1 << 2,
  kw_char = 1 << 4,
  kw_int = 1 << 6,
  kw_short = 1 << 8,
  kw_long = 1 << 10,
  other = 1 << 12,
};

static auto is_valid(int mask) -> bool {
  switch (mask) {
  case kw_void:

  case kw_bool:

  case kw_char:

  case kw_short:
  case kw_short + kw_int:

  case kw_int:

  case kw_long:
  case kw_long + kw_int:
  case kw_long + kw_long:
  case kw_long + kw_long + kw_int:

  case other:
    return true;
  default:
    return false;
  }
}

auto Parser::is_typename(Token *token) -> bool {
  switch (token->kind) {
  case TokenKind::kw_void:
  case TokenKind::kw_enum:
  case TokenKind::kw_bool:
  case TokenKind::kw_char:
  case TokenKind::kw_int:
  case TokenKind::kw_short:
  case TokenKind::kw_long:
  case TokenKind::kw_struct:
  case TokenKind::kw_union:
  case TokenKind::kw_typedef:
  case TokenKind::kw_static:
    return true;
  case TokenKind::identifier:
    return find_type(token->inner);
  default:
    return false;
  }
}

auto Parser::declspec(VarAttr *attr) -> Type * {
#define case_goto(value, target, update)                                       \
  case value:                                                                  \
    mask += target;                                                            \
    if (!is_valid(mask))                                                       \
      context_->fatal(position_->location, "");                                \
    update;                                                                    \
    goto loop

#define attr_goto(value, target, update)                                       \
  case value:                                                                  \
    attr->target = true;                                                       \
    update;                                                                    \
    goto loop

  Type *result = nullptr;
  int mask = 0;
loop:
  switch (next_kind()) {
    case_goto(TokenKind::kw_void, kw_void, consume());
    case_goto(TokenKind::kw_bool, kw_bool, consume());
    case_goto(TokenKind::kw_char, kw_char, consume());
    case_goto(TokenKind::kw_short, kw_short, consume());
    case_goto(TokenKind::kw_int, kw_int, consume());
    case_goto(TokenKind::kw_long, kw_long, consume());
    case_goto(TokenKind::kw_struct, other, result = struct_or_union_decl());
    case_goto(TokenKind::kw_union, other, result = struct_or_union_decl());
    case_goto(TokenKind::kw_enum, other, result = enum_spec());

    attr_goto(TokenKind::kw_typedef, is_typedef, consume());
    attr_goto(TokenKind::kw_static, is_static, consume());

  case TokenKind::identifier: {
    auto type = find_type(position_->inner);
    if (type && mask == 0) {
      mask += other;
      consume();
      result = type;
      goto loop;
    }
  }
  default:
    if (!is_valid(mask))
      context_->fatal(position_->location, "");
    break;
  }
  switch (mask) {
  case kw_void:
    return context_->void_type();
  case kw_bool:
    return context_->boolean();
  case kw_char:
    return context_->int8();
  case kw_short:
  case kw_short + kw_int:
    return context_->int16();
  case kw_int:
    return context_->int32();
  case kw_long:
  case kw_long + kw_int:
  case kw_long + kw_long:
  case kw_long + kw_long + kw_int:
    return context_->int64();
  default:
    assert(result);
    return result;
  }
}

auto Parser::pointers(Type *base) -> Type * {
  while (consume_if(TokenKind::star)) {
    base = context_->pointer_to(base);
  }
  return base;
}

auto Parser::array_dimensions(Type *base) -> Type * {
  auto open = consume(TokenKind::open_bracket);
  if (consume_if(TokenKind::close_bracket)) {
    base = suffix_type(base, nullptr);
    return context_->array_of(base, -1);
  } else if (int64_t tmp; !const_int(&tmp)) {
    context_->fatal(open->location, "");
  } else {
    int length = static_cast<int>(tmp);
    consume(TokenKind::close_bracket);
    base = suffix_type(base, nullptr);
    return context_->array_of(base, length);
  }
}

auto Parser::function_parameters(Type *base, ParamNames *param_names)
    -> Type * {
  if (param_names) {
    param_names->clear();
  }
  consume(TokenKind::open_paren);
  std::vector<Type *> params;

  while (!consume_if(TokenKind::close_paren)) {
    if (!params.empty()) {
      consume(TokenKind::comma);
    }
    auto base = declspec();
    auto [name, param] = declarator(base);
    if (param_names) {
      param_names->push_back(name);
    }
    if (param->kind == TypeKind::array) {
      auto array = cast<ArrayType>(param);
      param = context_->pointer_to(array->base);
    }
    params.push_back(param);
  }
  return context_->function_type(base, std::move(params));
}

auto Parser::suffix_type(Type *base, ParamNames *param_names) -> Type * {
  switch (next_kind()) {
  case TokenKind::open_paren:
    return function_parameters(base, param_names);
  case TokenKind::open_bracket:
    return array_dimensions(base);
  default:
    return base;
  }
}

auto Parser::declarator(Type *base, ParamNames *param_names)
    -> std::pair<Token *, Type *> {
  base = pointers(base);
  switch (next_kind()) {
  case TokenKind::identifier: {
    auto name = consume();
    auto type = suffix_type(base, param_names);
    return std::pair(name, type);
  }
  case TokenKind::open_paren: {
    auto start = position_;
    position_ += position_->inner + 1;

    base = suffix_type(base, param_names);
    auto end = position_;

    position_ = start + 1;
    auto result = declarator(base, param_names);

    position_ = end;
    return result;
  }
  default:
    return std::pair(position_, base);
  }
}

auto Parser::abstract_declarator(Type *base) -> Type * {
  base = pointers(base);
  if (!next_is(TokenKind::open_paren)) {
    return suffix_type(base, nullptr);
  } else {
    auto start = position_;
    position_ += position_->inner + 1;

    base = suffix_type(base, nullptr);
    auto end = position_;

    position_ = start + 1;
    auto result = abstract_declarator(base);

    position_ = end;
    return result;
  }
}

auto Parser::type_define(Type *base) -> void {
  bool is_first = true;
  while (!consume_if(TokenKind::semi)) {
    if (!is_first) {
      consume(TokenKind::comma);
    }
    is_first = false;
    auto [name, type] = declarator(base);
    create_typedef(name, type);
  }
}

auto Parser::declaration() -> Stmt * {
  VarAttr attr{};
  auto base = declspec(&attr);
  if (attr.is_typedef) {
    type_define(base);
    return context_->block_stmt({});
  }
  std::vector<Stmt *> stmts;
  bool is_first = true;
  while (!consume_if(TokenKind::semi)) {
    if (!is_first) {
      consume(TokenKind::comma);
    }
    is_first = false;
    auto [name, type] = declarator(base);
    auto value = create_local(name, type);
    if (auto token = consume_if(TokenKind::equal)) {
      auto lhs = context_->value(value);

      auto rhs = init(value->type);
      if (value->type->kind == TypeKind::array) {
        auto array_type = cast<ArrayType>(value->type);
        auto array_init = cast<ArrayInit>(rhs);
        if (array_type->length == -1) {
          int length = array_init->children.size();
          value->type = context_->array_of(array_type->base, length);
        }
      }

      int64_t size_bytes = context_->size_of(value->type);
      auto zero_expr = context_->zero(lhs, size_bytes);
      stmts.push_back(context_->expr_stmt(zero_expr));

      if (auto expr = init_local(lhs, rhs, token->location)) {
        stmts.push_back(context_->expr_stmt(expr));
      }
    }
  }
  return context_->block_stmt(std::move(stmts));
}

auto Parser::global(Token *name, Type *base, Type *type) -> void {
  if (name->kind != TokenKind::identifier) {
    return;
  }
  while (true) {
    uint8_t *init_data = 0;
    if (auto token = consume_if(TokenKind::equal)) {
      auto init = this->init(type);

      if (type->kind == TypeKind::array) {
        auto array_type = cast<ArrayType>(type);
        auto array_init = cast<ArrayInit>(init);
        if (array_type->length == -1) {
          int length = array_init->children.size();
          type = context_->array_of(array_type->base, length);
        }
      }

      std::string buffer;
      buffer.resize(context_->size_of(type));
      auto data = (uint8_t *) &buffer[0];
      init_global(init, {data, buffer.size()}, token->location);

      auto index = context_->push_literal(std::move(buffer));
      auto view = context_->storage(index);
      init_data = (uint8_t *) &view[0];
    }
    create_global(name, type, init_data);
    if (!consume_if(TokenKind::comma)) {
      consume(TokenKind::semi);
      break;
    } else if (consume_if(TokenKind::semi)) {
      break;
    } else {
      std::tie(name, type) = declarator(base);
    }
  }
}

auto Parser::function(Token *name, Type *type, ParamNames param_names,
                      Linkage linkage) -> void {
  create_declaration(name, type);
  if (consume_if(TokenKind::semi)) {
    return;
  }
  std::vector<LValue *> params;
  auto function_type = cast<FunctionType>(type);
  auto param_types = function_type->params;

  stack_size_ = 0;
  max_stack_size_ = 0;
  lable_map_.clear();

  entry_scope();
  for (int i = 0; i < param_types.size(); i++) {
    auto var = create_local(param_names[i], param_types[i]);
    params.push_back(var);
  }

  auto stmt = block_stmt();
  leave_scope();
  create_function(name, type, max_stack_size_, stmt, std::move(params),
                  linkage);
}

} // namespace lzhcc