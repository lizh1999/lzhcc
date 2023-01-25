#include "lzhcc_parse.h"
#include <cassert>
#include <cstring>

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
  while (!consume_if(TokenKind::close_brace)) {
    auto ident = consume(TokenKind::identifier);
    if (auto token = consume_if(TokenKind::equal)) {
      if (int64_t tmp; !const_int(&tmp)) {
        context_->fatal(token->location, "");
      } else {
        value = static_cast<int>(tmp);
      }
    }
    create_enum(ident, value++);
    if (!consume_if(TokenKind::comma)) {
      consume(TokenKind::close_brace);
      break;
    }
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
  int bits = 0;
  int align_bytes = 1;
  std::vector<Member> members;
  while (!consume_if(TokenKind::close_brace)) {
    VarAttr attr{};
    auto base = declspec(&attr);
    if (base->kind == TypeKind::record && consume_if(TokenKind::semi)) {
      int size = context_->size_of(base);
      int align = attr.align_bytes ?: context_->align_of(base);
      align_bytes = std::max(align_bytes, align);
      bits = align_to(bits, align * 8);
      members.push_back(Member{base, -1, bits / 8});
      bits += size * 8;
      continue;
    }
    bool is_first = true;
    while (!consume_if(TokenKind::semi)) {
      if (!is_first) {
        consume(TokenKind::comma);
      }
      is_first = false;
      auto [name, type] = declarator(base);

      bool is_bitfield = false;
      int bit_width = 0;
      if (auto token = consume_if(TokenKind::colon)) {
        is_bitfield = true;
        if (int64_t tmp; const_int(&tmp)) {
          bit_width = tmp;
        } else {
          context_->fatal(token->location, "");
        }
      }

      bool is_flexible = false;
      if (type->kind == TypeKind::array) {
        auto array = cast<ArrayType>(type);
        if (array->length == -1) {
          is_flexible = true;
        }
      }
      int size = is_flexible ? 0 : context_->size_of(type);
      int align = attr.align_bytes ?: context_->align_of(type);
      align_bytes = std::max(align_bytes, align);
      if (is_bitfield) {
        int type_bits = size * 8;
        if (bit_width == 0) {
          bits = align_to(bits, type_bits);
          continue;
        }
        if (bits / type_bits != (bits + bit_width - 1) / type_bits) {
          bits = align_to(bits, type_bits);
        }
        int offset = align_down(bits / 8, size);
        int bit_offset = bits % type_bits;
        bits += bit_width;
        members.push_back(
            Member{type, name->inner, offset, true, bit_offset, bit_width});
      } else {
        bits = align_to(bits, align * 8);
        members.push_back(Member{type, name->inner, bits / 8});
        bits += size * 8;

        if (is_flexible) {
          consume(TokenKind::semi);
          if (!next_is(TokenKind::close_brace)) {
            context_->fatal(position_->location, "");
          }
          break;
        }
      }
    }
  }
  int size_bytes = align_to(bits, align_bytes * 8) / 8;
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
     VarAttr attr{};
    auto base = declspec(&attr);
    if (base->kind == TypeKind::record && consume_if(TokenKind::semi)) {
      int size = context_->size_of(base);
      int align = attr.align_bytes ?: context_->align_of(base);
      align_bytes = std::max(align_bytes, align);
      members.push_back(Member{base, -1, 0});
      continue;
    }
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
  type->is_union = true;
}

enum {
  kw_void = 1 << 0,
  kw_bool = 1 << 2,
  kw_char = 1 << 4,
  kw_int = 1 << 6,
  kw_short = 1 << 8,
  kw_long = 1 << 10,
  other = 1 << 12,
  kw_signed = 1 << 14,
  kw_unsigned = 1 << 16,
  kw_float = 1 << 18,
  kw_double = 1 << 20,
};

static auto is_valid(int mask) -> bool {
  switch (mask) {
  case kw_void:

  case kw_bool:

  case kw_signed + kw_char:

  case kw_char:
  case kw_unsigned + kw_char:

  case kw_short:
  case kw_short + kw_int:
  case kw_signed + kw_short:
  case kw_signed + kw_short + kw_int:

  case kw_unsigned + kw_short:
  case kw_unsigned + kw_short + kw_int:

  case kw_int:
  case kw_signed:
  case kw_signed + kw_int:

  case kw_unsigned:
  case kw_unsigned + kw_int:

  case kw_long:
  case kw_long + kw_int:
  case kw_long + kw_long:
  case kw_long + kw_long + kw_int:

  case kw_signed + kw_long:
  case kw_signed + kw_long + kw_int:
  case kw_signed + kw_long + kw_long:
  case kw_signed + kw_long + kw_long + kw_int:

  case kw_unsigned + kw_long:
  case kw_unsigned + kw_long + kw_int:
  case kw_unsigned + kw_long + kw_long:
  case kw_unsigned + kw_long + kw_long + kw_int:

  case kw_float:

  case kw_double:

  case kw_long + kw_double:

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
  case TokenKind::kw_extern:
  case TokenKind::kw_alignas:
  case TokenKind::kw_signed:
  case TokenKind::kw_unsigned:
  case TokenKind::kw_auto:
  case TokenKind::kw_const:
  case TokenKind::kw_noreturn:
  case TokenKind::kw_register:
  case TokenKind::kw_restrict:
  case TokenKind::kw_volatile:
  case TokenKind::kw_float:
  case TokenKind::kw_double:
    return true;
  case TokenKind::identifier:
    return find_type(token->inner);
  default:
    return false;
  }
}

auto Parser::declspec(VarAttr *attr) -> Type * {
#define case_goto(value, op, target, update)                                   \
  case value:                                                                  \
    mask op target;                                                            \
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
    case_goto(TokenKind::kw_void, +=, kw_void, consume());
    case_goto(TokenKind::kw_bool, +=, kw_bool, consume());
    case_goto(TokenKind::kw_char, +=, kw_char, consume());
    case_goto(TokenKind::kw_short, +=, kw_short, consume());
    case_goto(TokenKind::kw_int, +=, kw_int, consume());
    case_goto(TokenKind::kw_long, +=, kw_long, consume());
    case_goto(TokenKind::kw_signed, |=, kw_signed, consume());
    case_goto(TokenKind::kw_unsigned, |=, kw_unsigned, consume());
    case_goto(TokenKind::kw_struct, +=, other, result = struct_or_union_decl());
    case_goto(TokenKind::kw_union, +=, other, result = struct_or_union_decl());
    case_goto(TokenKind::kw_enum, +=, other, result = enum_spec());
    case_goto(TokenKind::kw_float, +=, kw_float, consume());
    case_goto(TokenKind::kw_double, +=, kw_double, consume());

    attr_goto(TokenKind::kw_typedef, is_typedef, consume());
    attr_goto(TokenKind::kw_static, is_static, consume());
    attr_goto(TokenKind::kw_extern, is_extern, consume());

  case TokenKind::kw_auto:
  case TokenKind::kw_const:
  case TokenKind::kw_noreturn:
  case TokenKind::kw_register:
  case TokenKind::kw_restrict:
  case TokenKind::kw_volatile:
    consume();
    goto loop;

  case TokenKind::kw_alignas: {
    auto token = consume();
    consume(TokenKind::open_paren);
    int align_bytes;
    if (is_typename(position_)) {
      auto type = abstract_declarator(declspec());
      align_bytes = context_->align_of(type);
    } else if (int64_t tmp; const_int(&tmp)) {
      align_bytes = tmp;
    } else {
      context_->fatal(token->location, "");
    }
    attr->align_bytes = align_bytes;
    consume(TokenKind::close_paren);
    goto loop;
  }

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
  case kw_signed + kw_char:
    return context_->int8();
  case kw_char:
  case kw_unsigned + kw_char:
    return context_->uint8();
  case kw_short:
  case kw_short + kw_int:
  case kw_signed + kw_short:
  case kw_signed + kw_short + kw_int:
    return context_->int16();
  case kw_unsigned + kw_short:
  case kw_unsigned + kw_short + kw_int:
    return context_->uint16();
  case kw_int:
  case kw_signed:
  case kw_signed + kw_int:
    return context_->int32();
  case kw_unsigned:
  case kw_unsigned + kw_int:
    return context_->uint32();
  case kw_long:
  case kw_long + kw_int:
  case kw_long + kw_long:
  case kw_long + kw_long + kw_int:
  case kw_signed + kw_long:
  case kw_signed + kw_long + kw_int:
  case kw_signed + kw_long + kw_long:
  case kw_signed + kw_long + kw_long + kw_int:
    return context_->int64();
  case kw_unsigned + kw_long:
  case kw_unsigned + kw_long + kw_int:
  case kw_unsigned + kw_long + kw_long:
  case kw_unsigned + kw_long + kw_long + kw_int:
    return context_->uint64();
  case kw_float:
    return context_->float32();
  case kw_double:
  case kw_long + kw_double:
    return context_->float64();
  default:
    assert(result);
    return result;
  }
}

auto Parser::pointers(Type *base) -> Type * {
loop:
  switch (next_kind()) {
  case TokenKind::kw_const:
  case TokenKind::kw_restrict:
  case TokenKind::kw_volatile:
    consume();
    goto loop;
  case TokenKind::star:
    consume();
    base = context_->pointer_to(base);
    goto loop;
  default:
    return base;
  }
}

auto Parser::array_dimensions(Type *base) -> Type * {
  auto open = consume(TokenKind::open_bracket);
loop:
  switch (next_kind()) {
  case TokenKind::kw_static:
  case TokenKind::kw_restrict:
    consume();
    goto loop;
  case TokenKind::close_bracket:
    consume();
    base = suffix_type(base, nullptr);
    return context_->array_of(base, -1);
  default:
    if (int64_t tmp; !const_int(&tmp)) {
      context_->fatal(open->location, "");
    } else {
      int length = static_cast<int>(tmp);
      consume(TokenKind::close_bracket);
      base = suffix_type(base, nullptr);
      return context_->array_of(base, length);
    }
  }
}

auto Parser::function_parameters(Type *base, ParamNames *param_names)
    -> Type * {
  if (param_names) {
    param_names->clear();
  }
  consume(TokenKind::open_paren);
  if (next_is(TokenKind::kw_void) &&
      position_[1].kind == TokenKind::close_paren) {
    position_ += 2;
    return context_->function_type(base, {}, false);
  }

  std::vector<Type *> params;
  bool is_variadic = false;

  while (!consume_if(TokenKind::close_paren)) {
    if (!params.empty()) {
      consume(TokenKind::comma);
    }
    if (consume_if(TokenKind::dotdotdot)) {
      is_variadic = true;
      consume(TokenKind::close_paren);
      break;
    }
    auto base = declspec();
    auto [name, param] = declarator(base);
    if (param_names) {
      param_names->push_back(name);
    }
    if (param->kind == TypeKind::array) {
      auto array = cast<ArrayType>(param);
      param = context_->pointer_to(array->base);
    } else if (param->kind == TypeKind::function) {
      param = context_->pointer_to(param);
    }
    params.push_back(param);
  }
  return context_->function_type(base, std::move(params), is_variadic);
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
    if (type->kind == TypeKind::function) {
      create_declaration(name, type);
      consume(TokenKind::semi);
      break;
    }
    if (attr.is_extern) {
      create_declaration(name, type);
      continue;
    }
    if (attr.is_static) {
      global(name, base, type, &attr);
      break;
    }
    int align_bytes = context_->align_of(type);
    if (attr.align_bytes != 0) {
      if (attr.align_bytes < 0 || attr.align_bytes % align_bytes != 0) {
        context_->fatal(position_->location, "");
      }
      align_bytes = attr.align_bytes;
    }
    if (auto token = consume_if(TokenKind::equal)) {
      auto rhs = init(type);
      if (type->kind == TypeKind::array) {
        auto array_type = cast<ArrayType>(type);
        auto array_init = cast<ArrayInit>(rhs);
        if (array_type->length == -1) {
          int length = array_init->children.size();
          type = context_->array_of(array_type->base, length);
        }
      }
      auto value = create_local(name, type, align_bytes);
      auto lhs = context_->value(value);

      int64_t size_bytes = context_->size_of(value->type);
      auto zero_expr = context_->zero(lhs, size_bytes);
      stmts.push_back(context_->expr_stmt(zero_expr));

      if (auto expr = init_local(lhs, rhs, token->location)) {
        stmts.push_back(context_->expr_stmt(expr));
      }
    } else {
      create_local(name, type, align_bytes);
    }
  }
  return context_->block_stmt(std::move(stmts));
}

auto Parser::global(Token *name, Type *base, Type *type, VarAttr *attr)
    -> void {
  if (name->kind != TokenKind::identifier) {
    consume(TokenKind::semi);
    return;
  }
  auto linkage = attr->is_static ? Linkage::internal : Linkage::external;
  while (true) {
    int align_bytes = attr->align_bytes ?: context_->align_of(type);
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

      do {
        if (type->kind != TypeKind::record) {
          break;
        }
        auto record_type = cast<RecordType>(type);
        auto record_init = cast<RecordInit>(init);
        if (record_type->members.empty()) {
          break;
        }
        auto last_type = record_type->members.back().type;
        if (last_type->kind != TypeKind::array) {
          break;
        }
        auto array_type = cast<ArrayType>(last_type);
        if (array_type->length != -1) {
          break;
        }
        auto &list = record_init->children;
        auto it = std::find_if(list.begin(), list.end(), [&](auto &&arg) {
          return arg.first == &record_type->members.back();
        });
        if (it == list.end()) {
          break;
        }
        auto array_init = cast<ArrayInit>(it->second);
        auto new_type = context_->record_type();
        new_type->members = record_type->members;
        new_type->size_bytes = record_type->size_bytes;
        new_type->align_bytes = record_type->align_bytes;

        int length = array_init->children.size();
        auto new_last = context_->array_of(array_type->base, length);

        new_type->members.back().type = new_last;
        new_type->size_bytes += context_->size_of(new_last);

        type = new_type;
      } while (false);

      std::string buffer;
      buffer.resize(context_->size_of(type));
      auto data = (uint8_t *)&buffer[0];

      std::vector<Relocation> relocations;
      init_global(init, {data, buffer.size()}, relocations, token->location);

      for (auto &rel : relocations) {
        rel.index = reinterpret_cast<uint8_t *>(rel.index) - data;
      }

      auto index = context_->push_literal(std::move(buffer));
      auto view = context_->storage(index);
      auto init_data = (uint8_t *)&view[0];
      create_global(name, type, init_data, std::move(relocations), align_bytes,
                    linkage);
    } else if (attr->is_extern) {
      create_declaration(name, type);
    } else {
      create_global(name, type, 0, {}, align_bytes, linkage);
    }

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
    int align = context_->align_of(param_types[i]);
    auto var = create_local(param_names[i], param_types[i], align);
    params.push_back(var);
  }

  LValue *va_area = nullptr;
  if (function_type->is_variadic) {
    auto type = context_->pointer_to(context_->void_type());
    va_area = create_local("__va_area__", type);
  }

  {
    auto function_name = context_->c_str(name->inner);
    int length = strlen(function_name);
    auto name_type = context_->array_of(context_->int8(), length + 1);
    auto value = create_anon_global(name_type, (uint8_t *)function_name, {});
    int func1 = context_->push_identifier("__func__");
    int func2 = context_->push_identifier("__FUNCTION__");
    scopes_.back().var_map.emplace(func1, value);
    scopes_.back().var_map.emplace(func2, value);
  }

  auto stmt = block_stmt();
  leave_scope();
  create_function(name, type, max_stack_size_, stmt, std::move(params), va_area,
                  linkage);
}

} // namespace lzhcc