#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::operator()() -> Ast {
  current_ = &file_scope_;
  std::vector<Global *> globals;
  std::vector<Function *> functions;
  globals_ = &globals;

  while (next_kind() != TokenKind::eof) {
    auto base = declspec();
    auto [name, type] = declarator(base);
    auto visitor = overloaded{
        [&, name = name, type = type](FunctionType &) {
          functions.push_back(function(name, type));
        },
        [&, name = name, type = type](auto &&) {
          auto vars = global(name, base, type);
          globals.insert(globals.end(), vars.begin(), vars.end());
        },
    };
    std::visit(visitor, *type);
  }
  return Ast{std::move(globals), std::move(functions)};
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
  current_ = context_->create<Scope>(Scope{current_, {}});
}

auto Parser::leave_scope() -> void {
  for (auto [_, var] : current_->var_map) {
    auto visitor = overloaded{
        [&](Local *local) { stack_size -= context_->size_of(local->type); },
        [](auto &&) {}};
    std::visit(visitor, var);
  }
  current_ = current_->parent;
}

auto Parser::find_var(const Token *token) -> Variable {
  auto scope = current_;
  for (; scope; scope = scope->parent) {
    auto it = scope->var_map.find(token->inner);
    if (it != scope->var_map.end()) {
      return it->second;
    }
  }
  context_->fatal(token->location, "");
}

auto Parser::create_local(const Token *token, const Type *type) -> Local * {
  if (current_->var_map.contains(token->inner)) {
    context_->fatal(token->location, "");
  }
  auto var = create<Local>(Local{stack_size, type});
  stack_size += context_->size_of(type);
  max_stack_size = std::max(max_stack_size, stack_size);
  current_->var_map.emplace(token->inner, var);
  return var;
}

auto Parser::create_global(const Token *token, const Type *type, int init)
    -> Global * {
  if (file_scope_.var_map.contains(token->inner)) {
    context_->fatal(token->location, "");
  }
  auto name = context_->identifier(token->inner);
  auto var = create<Global>(Global{name, type, init});
  file_scope_.var_map.emplace(token->inner, var);
  return var;
}

auto Parser::create_anon(const Type *type, int init) -> Global * {
  int ident = -unique_id_;
  auto var = create<Global>(Global{unique_name(), type, init});
  file_scope_.var_map.emplace(ident, var);
  globals_->push_back(var);
  return var;
}

auto Parser::unique_name() -> std::string_view {
  std::string name = ".L.." + std::to_string(unique_id_++);
  int index = context_->push_literal(std::move(name));
  return context_->literal(index);
}

} // namespace lzhcc