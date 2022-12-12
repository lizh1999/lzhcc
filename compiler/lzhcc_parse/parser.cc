#include "lzhcc.h"
#include "lzhcc_parse.h"
#include <cassert>

namespace lzhcc {

auto Parser::operator()() -> Statement * {
  auto ret = statement();
  while (next_kind() != TokenKind::eof) {
    ret = statement();
  }
  return ret;
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

auto Parser::get_or_allocate(int identifier) -> Variable * {
  if (auto it = var_map_.find(identifier); it != var_map_.end()) {
    return it->second;
  } else {
    auto var = create<Variable>(Variable{(int)var_map_.size() * 8, context_->int64()});
    var_map_.emplace(identifier, var);
    return var;
  }
}

} // namespace lzhcc