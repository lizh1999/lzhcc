#include "lzhcc.h"
#include "lzhcc_parse.h"

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

auto Parser::declarator(Type *base) -> Variable * {
  auto type = pointers(base);
  auto name = consume(TokenKind::identifier);
  return create_var(name, type);
}

auto Parser::declaration() -> std::vector<Statement *> {
  auto base = declspec();
  auto var = declarator(base);

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
    var = declarator(base);
    goto loop;
  case TokenKind::semi:
    consume();
    return stmts;
  default:
    context_->fatal(position_->location, "");
  }
}

} // namespace lzhcc