#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::operator()() -> std::vector<Function *> {
  scope_ = nullptr;
  std::vector<Function *> functions;
  while (next_kind() != TokenKind::eof) {
    functions.push_back(function());
  }
  return functions;
}

auto Parser::next_kind() const -> TokenKind { return position_->kind; }

auto Parser::consume() -> const Token * {
  assert(position_->kind != TokenKind::eof);
  return position_++;
}

auto Parser::consume(TokenKind kind) -> const Token * {
  if (position_->kind != kind) {
    context_->fatal(position_->location, "");
  }
  return position_++;
}

auto Parser::consume_if(TokenKind kind) -> const Token * {
  if (position_->kind != kind) {
    return nullptr;
  } else {
    return position_++;
  }
}

auto Parser::entry_scope() -> void {
  scope_ = context_->create<Scope>(Scope{scope_, {}});
}

auto Parser::leave_scope() -> void {
  for (auto [_, var] : scope_->var_map) {
    stack_size -= std::visit(size_of, *var->type);
  }
  scope_ = scope_->parent;
}

auto Parser::find_var(const Token *token) -> Local * {
  auto scope = scope_;
  for (; scope; scope = scope->parent) {
    auto it = scope->var_map.find(token->inner);
    if (it != scope->var_map.end()) {
      return it->second;
    }
  }
  return nullptr;
}

auto Parser::create_var(std::pair<const Token*, Type *> x) -> Local* {
  return create_var(x.first, x.second);
}

auto Parser::create_var(const Token *token, const Type *type) -> Local * {
  if (find_var(token)) {
    context_->fatal(token->location, "");
  }
  auto var = create<Local>(Local{stack_size, type});
  stack_size += std::visit(size_of, *type);
  max_stack_size = std::max(max_stack_size, stack_size);
  scope_->var_map.emplace(token->inner, var);
  return var;
}

} // namespace lzhcc