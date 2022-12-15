#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::operator()() -> Module {
  scopes_.emplace_back();
  while (next_kind() != TokenKind::eof) {
    auto base = declspec();
    ParamNames param_names;
    auto [name, type] = declarator(base, &param_names);
    if (type->kind == TypeKind::function) {
      function(name, type, std::move(param_names));
    } else {
      global(name, base, type);
    }
  }
  std::vector<GValue *> gvalues;
  std::vector<Function *> functions;
  for (auto [_, var] : scopes_.front().var_map) {
    switch (var->kind) {
    case ValueKind::global:
      gvalues.push_back(cast<GValue>(var));
      break;
    case ValueKind::function:
      functions.push_back(cast<Function>(var));
      break;
    case ValueKind::local:
      assert(false);
    }
  }

  return Module{std::move(gvalues), std::move(functions)};
}

auto Parser::next_kind() -> TokenKind { return position_->kind; }

auto Parser::consume() -> Token * {
  assert(position_->kind != TokenKind::eof);
  return position_++;
}

auto Parser::consume(TokenKind kind) -> Token * {
  if (position_->kind != kind) {
    context_->fatal(position_->location, "");
  }
  return position_++;
}

auto Parser::consume_if(TokenKind kind) -> Token * {
  if (position_->kind != kind) {
    return nullptr;
  } else {
    return position_++;
  }
}

auto Parser::entry_scope() -> void {
  Scope *parent = &scopes_.back();
  scopes_.push_back({parent, {}});
}

auto Parser::leave_scope() -> void {
  auto &current = scopes_.back();
  for (auto [_, var] : current.var_map) {
    if (var->kind == ValueKind::local) {
      stack_size_ -= context_->size_of(var->type);
    }
  }
  scopes_.pop_back();
  if (scopes_.size() == 1) {
    assert(stack_size_ == 0);
  }
}

auto Parser::find_var(Token *token) -> Value * {
  auto scope = &scopes_.back();
  for (; scope; scope = scope->parent) {
    auto it = scope->var_map.find(token->inner);
    if (it != scope->var_map.end()) {
      return it->second;
    }
  }
  context_->fatal(token->location, "");
}

auto Parser::create_local(Token *token, Type *type) -> LValue * {
  auto &current = scopes_.back();
  if (current.var_map.contains(token->inner)) {
    context_->fatal(token->location, "");
  }
  auto var = context_->create_local(type, stack_size_);
  stack_size_ += context_->size_of(type);
  max_stack_size_ = std::max(max_stack_size_, stack_size_);
  current.var_map.emplace(token->inner, var);
  return var;
}

auto Parser::create_global(Token *token, Type *type, uint8_t *init) -> void {
  auto &file_scope = scopes_.front();
  if (file_scope.var_map.contains(token->inner)) {
    context_->fatal(token->location, "");
  }
  auto name = context_->storage(token->inner);
  auto var = context_->create_global(type, name, init);
  file_scope.var_map.emplace(token->inner, var);
}

auto Parser::create_function(Token *token, Type *type, int stack_size,
                             Stmt *stmt, std::vector<LValue *> params) -> void {
  auto &file_scope = scopes_.front();
  if (file_scope.var_map.contains(token->inner)) {
    context_->fatal(token->location, "");
  }
  auto name = context_->storage(token->inner);
  auto var = context_->create_function(type, name, stack_size, stmt,
                                       std::move(params));
  file_scope.var_map.emplace(token->inner, var);
}

auto Parser::create_anon(Type *type, uint8_t *init) -> GValue * {
  auto &file_scope = scopes_.front();
  auto [name, uid] = unique_name();
  auto var = context_->create_global(type, name, init);
  file_scope.var_map.emplace(uid, var);
  return var;
}

auto Parser::unique_name() -> std::pair<std::string_view, int> {
  int uid = unique_id_++;
  std::string name = ".L.." + std::to_string(uid);
  int index = context_->push_literal(std::move(name));
  return {context_->storage(index), -uid};
}

} // namespace lzhcc