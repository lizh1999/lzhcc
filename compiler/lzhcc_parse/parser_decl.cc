#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>
#include <type_traits>
#include <charconv>
#include <variant>

namespace lzhcc {

auto Parser::declspec() -> Type * {
  consume(TokenKind::kw_int);
  return context_->int64();
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
  auto text = context_->literal(token->inner);
  int length;
  std::from_chars(text.begin(), text.end(), length);
  consume(TokenKind::close_bracket);
  return create<Type>(ArrayType{base, length});
}

auto Parser::function_parameters(Type *base) -> Type * {
  consume(TokenKind::open_paren);
  std::vector<const Token *> names;
  std::vector<Type *> paramters;

  while (!consume_if(TokenKind::close_paren)) {
    if (!paramters.empty()) {
      consume(TokenKind::comma);
    }
    auto base = declspec();
    auto [name, paramter] = declarator(base);
    names.push_back(name);
    paramters.push_back(paramter);
  }
  return create<Type>(
      FunctionType{base, std::move(names), std::move(paramters)});
}

auto Parser::suffix_type(Type *base) -> Type * {
  switch (next_kind()) {
  case TokenKind::open_paren:
    return function_parameters(base);
  case TokenKind::open_bracket:
    return array_dimensions(base);
  default:
    return base;
  }
}

auto Parser::declarator(Type *base) -> std::pair<const Token *, Type *> {
  base = pointers(base);
  auto name = consume(TokenKind::identifier);
  auto type = suffix_type(base);
  return std::pair(name, type);
}

auto Parser::declaration() -> std::vector<Statement *> {
  auto base = declspec();
  auto var = create_var(declarator(base));

  std::vector<Statement *> stmts;
loop:
  switch (next_kind()) {
  case TokenKind::equal: {
    consume();
    auto lhs = create<VarRefExpr>(var);
    auto rhs = assignment();
    auto expr = create<BinaryExpr>(BinaryKind::assign, var->type, lhs, rhs);
    stmts.push_back(create<ExpressionStmt>(expr));
    goto loop;
  }
  case TokenKind::comma:
    consume();
    var = create_var(declarator(base));
    goto loop;
  case TokenKind::semi:
    consume();
    return stmts;
  default:
    context_->fatal(position_->location, "");
  }
}

auto Parser::function() -> Function * {
  auto base = declspec();
  auto [name, type] = declarator(base);

  std::vector<Local *> paramters;
  auto visitor = [&, name = name](auto &&arg) {
    using T = std::decay_t<decltype(arg)>;
    if constexpr (std::is_same_v<T, FunctionType>) {
      int n = arg.paramters.size();
      for (int i = 0; i < n; i++) {
        auto var = create_var(arg.names[i], arg.paramters[i]);
        paramters.push_back(var);
      }
    } else {
      context_->fatal(name->location, "");
    }
  };

  assert(!scope_);
  stack_size = 0;
  max_stack_size = 0;

  entry_scope();
  std::visit(visitor, *type);
  auto stmt = block_stmt(/*is_top=*/true);
  return create<Function>(
      Function{name, max_stack_size, stmt, type, std::move(paramters)});
}

} // namespace lzhcc