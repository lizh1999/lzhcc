#include "lzhcc_parse.h"
#include <charconv>

namespace lzhcc {

auto Parser::struct_decl() -> Type * {
  consume(TokenKind::kw_struct);
  auto name = consume_if(TokenKind::identifier);
  if (name && !next_is(TokenKind::open_brace)) {
    if (auto type = find_tag(name->inner)) {
      return type;
    } else {
      context_->fatal(name->location, "");
    }
  }
  entry_scope();
  consume(TokenKind::open_brace);
  int offset = 0;
  int align_bytes = 1;
  std::unordered_map<int, Member> member_map;
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
      member_map.emplace(name->inner, Member{type, offset});
      offset += size;
      align_bytes = std::max(align_bytes, align);
    }
  }
  leave_scope();
  int size_bytes = align_to(offset, align_bytes);
  auto type =
      context_->record_type(std::move(member_map), size_bytes, align_bytes);
  if (name) {
    create_tag(name, type);
  }
  return type;
}

auto Parser::union_decl() -> Type * {
  consume(TokenKind::kw_union);
  auto name = consume_if(TokenKind::identifier);
  if (name && !next_is(TokenKind::open_brace)) {
    if (auto type = find_tag(name->inner)) {
      return type;
    } else {
      context_->fatal(name->location, "");
    }
  }
  entry_scope();
  consume(TokenKind::open_brace);
  int size_bytes = 0;
  int align_bytes = 1;
  std::unordered_map<int, Member> member_map;
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
      member_map.emplace(name->inner, Member{type, 0});
      size_bytes = std::max(size_bytes, size);
      align_bytes = std::max(align_bytes, align);
    }
  }
  leave_scope();
  size_bytes = align_to(size_bytes, align_bytes);
  auto type =
      context_->record_type(std::move(member_map), size_bytes, align_bytes);
  if (name) {
    create_tag(name, type);
  }
  return type;
}

enum {
  kw_void = 1 << 0,
  kw_char = 1 << 2,
  kw_int = 1 << 4,
  kw_short = 1 << 6,
  kw_long = 1 << 8,
  other = 1 << 10,
};

static auto is_valid(int mask) -> bool {
  switch (mask) {
  case kw_void:

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

auto Parser::declspec() -> Type * {
#define case_goto(value, target, update)                                       \
  case value:                                                                  \
    mask += target;                                                            \
    if (!is_valid(mask))                                                       \
      context_->fatal(position_->location, "");                                \
    update;                                                                    \
    goto loop

  Type *result = nullptr;
  int mask = 0;
loop:
  switch (next_kind()) {
    case_goto(TokenKind::kw_void, kw_void, consume());
    case_goto(TokenKind::kw_char, kw_char, consume());
    case_goto(TokenKind::kw_short, kw_short, consume());
    case_goto(TokenKind::kw_int, kw_int, consume());
    case_goto(TokenKind::kw_long, kw_long, consume());
    case_goto(TokenKind::kw_struct, other, result = struct_decl());
    case_goto(TokenKind::kw_union, other, result = union_decl());
  default:
    if (!is_valid(mask))
      context_->fatal(position_->location, "");
    break;
  }
  switch (mask) {
  case kw_void:
    return context_->void_type();
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
  consume(TokenKind::open_bracket);
  auto token = consume(TokenKind::numeric);
  auto text = context_->storage(token->inner);
  int length;
  std::from_chars(text.begin(), text.end(), length);
  consume(TokenKind::close_bracket);
  base = suffix_type(base, nullptr);
  return context_->array_of(base, length);
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

auto Parser::declaration() -> std::vector<Stmt *> {
  auto base = declspec();
  std::vector<Stmt *> stmts;
  bool is_first = true;
  while (!consume_if(TokenKind::semi)) {
    if (!is_first) {
      consume(TokenKind::comma);
    }
    is_first = false;
    auto [name, type] = declarator(base);
    auto var = create_local(name, type);
    if (consume_if(TokenKind::equal)) {
      auto lhs = context_->value(var);
      auto rhs = assignment();
      auto expr = context_->assign(var->type, lhs, rhs);
      stmts.push_back(context_->expr_stmt(expr));
    }
  }
  return stmts;
}

auto Parser::global(Token *name, Type *base, Type *type) -> void {
  create_global(name, type);
  while (true) {
    if (consume_if(TokenKind::comma)) {
      std::tie(name, type) = declarator(base);
      create_global(name, type);
    } else {
      consume(TokenKind::semi);
      break;
    }
  }
}

auto Parser::function(Token *name, Type *type, ParamNames param_names) -> void {
  create_declaration(name, type);
  if (consume_if(TokenKind::semi)) {
    return;
  }
  std::vector<LValue *> params;
  auto function_type = cast<FunctionType>(type);
  auto param_types = function_type->params;

  stack_size_ = 0;
  max_stack_size_ = 0;

  entry_scope();
  for (int i = 0; i < param_types.size(); i++) {
    auto var = create_local(param_names[i], param_types[i]);
    params.push_back(var);
  }

  auto stmt = block_stmt(/*is_top=*/true);
  create_function(name, type, max_stack_size_, stmt, std::move(params));
}

} // namespace lzhcc