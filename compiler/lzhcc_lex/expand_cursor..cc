#include "lzhcc_lex.h"
#include <cassert>
#include <iomanip>
#include <sstream>

namespace lzhcc {

auto ExpandCursor::into(std::vector<Token> tokens) -> Cursor {
  return [i = 0, tokens = std::move(tokens)]() mutable -> Token {
    if (i != tokens.size()) {
      return tokens[i++];
    } else {
      Token token;
      token.kind = TokenKind::eof;
      token.start_of_line = true;
      return token;
    }
  };
}

ExpandCursor::ExpandCursor(Cursor cursor, Context *context)
    : top_token_(cursor()), top_cursor_(std::move(cursor)), context_(context) {}

ExpandCursor::ExpandCursor(std::vector<Token> tokens, Context *context)
    : ExpandCursor(into(std::move(tokens)), context) {}

auto ExpandCursor::operator()() -> Token { return advance(); }

auto ExpandCursor::advance() -> Token {
  if (top_token_.kind == TokenKind::eof) {
    return top_token_;
  }
  if (top_token_.kind != TokenKind::identifier || top_token_.expand_disable) {
    auto result = top_token_;
    advance_top_token();
    return result;
  }
  auto macro = context_->find_macro(top_token_.inner);
  if (!macro || macro->expand_disable) {
    auto result = top_token_;
    advance_top_token();
    result.expand_disable = true;
    return result;
  }

  auto origin = top_token_;
  if (macro->kind == MacroKind::object) {
    advance_top_token();
    expand(cast<ObjectMacro>(macro), origin);
    return advance();
  } else if (macro->kind == MacroKind::builtin) {
    auto result = cast<BuiltinMacro>(macro)->handle(origin);
    advance_top_token();
    return result;
  }

  advance_top_token();
  if (top_token_.kind != TokenKind::open_paren) {
    return origin;
  }

  expand(cast<FunctionMacro>(macro), origin);
  return advance();
}

auto ExpandCursor::stringize(std::span<Token> in, Token hash) -> Token {
  std::string text;
  bool is_first = true;
  for (auto token : in) {
    if (!is_first && token.leading_space) {
      text.push_back(' ');
    }
    is_first = false;
    text.append(context_->to_string(token));
  }
  std::stringstream stream;
  stream << std::quoted(text);
  return Token{
      .kind = TokenKind::string,
      .leading_space = hash.leading_space,
      .start_of_line = hash.start_of_line,
      .expand_disable = false,
      .location = hash.location,
      .inner = context_->push_literal(stream.str()),
  };
}

auto ExpandCursor::paste(Token lhs, Token rhs) -> Token {
  std::string text;
  text.append(context_->to_string(lhs));
  text.append(context_->to_string(rhs));
  auto chars = context_->append_text(std::move(text), "");
  SourceCursor source(std::move(chars), context_);
  Token token = source();
  Token dummy = source();
  if (dummy.kind != TokenKind::eof) {
    context_->fatal(lhs.location, "");
  }
  token.location = lhs.location;
  token.leading_space = lhs.leading_space;
  token.start_of_line = lhs.start_of_line;
  return token;
}

auto ExpandCursor::expand(ObjectMacro *macro, Token origin) -> void {
  assert(!macro->expand_disable);
  std::vector<Token> result;
  for (auto token : macro->replace) {
    if (!result.empty() && result.back().kind == TokenKind::hash_hash) {
      result.pop_back();
      assert(!result.empty());
      auto lhs = result.back();
      auto rhs = result.back();
      result.back() = paste(lhs, rhs);
    } else {
      result.push_back(token);
    }
  }
  if (!result.empty()) {
    auto &first = result.front();
    first.leading_space = origin.leading_space;
    first.start_of_line = origin.start_of_line;
  }
  for (auto &token : result) {
    token.location = origin.location;
  }
  return push(macro, into(std::move(result)));
}

auto ExpandCursor::expand(FunctionMacro *macro, Token origin) -> void {
  assert(!macro->expand_disable);
  advance_top_token();

  std::vector<std::vector<Token>> args;
  if (macro->param.empty()) {
    if (top_token_.kind != TokenKind::close_paren) {
      context_->fatal(origin.location, "");
    }
    advance_top_token();
  } else {
    int nested_level = 1;
    std::vector<Token> arg;
    while (true) {
      auto consume = top_token_;
      advance_top_token();
      if (consume.kind == TokenKind::eof) {
        context_->fatal(origin.location, "");
      }
      if (consume.kind == TokenKind::open_paren) {
        ++nested_level;
      } else if (consume.kind == TokenKind::close_paren) {
        --nested_level;
      }
      if (nested_level == 0) {
        args.push_back(std::move(arg));
        break;
      }
      if (1 < nested_level || consume.kind != TokenKind::comma) {
        arg.push_back(consume);
        continue;
      }
      bool is_last_arg = args.size() + 1 == macro->param.size();
      if (macro->is_variadic && is_last_arg) {
        arg.push_back(consume);
      } else {
        args.push_back(std::move(arg));
      }
    }
  }
  if (args.size() + 1 == macro->param.size() && macro->is_variadic) {
    args.push_back({});
  }

  if (args.size() != macro->param.size()) {
    context_->fatal(origin.location, "");
  }

  std::vector<std::vector<Token>> expand(macro->param.size());
  for (int i = 0; i < args.size(); i++) {
    if (macro->param[i] != ParamKind::expand &&
        macro->param[i] != ParamKind::mixed) {
      continue;
    }
    ExpandCursor cursor(args[i], context_);
    for (auto token = cursor(); token.kind != TokenKind::eof;) {
      expand[i].push_back(token);
      token = cursor();
    }
  }

  std::vector<Token> result;
  for (auto token : macro->replace) {
    if (token.kind == TokenKind::hash || token.kind == TokenKind::hash_hash) {
      result.push_back(token);
      continue;
    }
    if (!result.empty() && result.back().kind == TokenKind::hash) {
      assert(token.kind == TokenKind::raw_arg);
      auto &arg = args[token.inner];
      token = stringize(arg, result.back());
      result.pop_back();
    }
    if (!result.empty() && result.back().kind == TokenKind::hash_hash) {
      result.pop_back();
      assert(!result.empty());
      if (result.back().kind == TokenKind::placeholder) {
        result.pop_back();
      } else if (token.kind == TokenKind::raw_arg) {
        auto &arg = args[token.inner];
        if (!arg.empty()) {
          auto lhs = result.back(), rhs = arg.front();
          result.back() = paste(lhs, rhs);
          result.insert(result.end(), arg.begin() + 1, arg.end());
        }
        continue;
      } else {
        auto lhs = result.back(), rhs = token;
        result.back() = paste(lhs, rhs);
        continue;
      }
    }

    if (token.kind == TokenKind::raw_arg) {
      auto &arg = args[token.inner];
      if (arg.empty()) {
        result.push_back({.kind = TokenKind::placeholder});
      } else {
        int first = result.size();
        result.insert(result.end(), arg.begin(), arg.end());
        if (!arg.empty()) {
          result[first].start_of_line = token.start_of_line;
          result[first].leading_space = token.leading_space;
        }
      }
    } else if (token.kind == TokenKind::expand_arg) {
      auto &arg = expand[token.inner];
      int first = result.size();
      result.insert(result.end(), arg.begin(), arg.end());
      if (!arg.empty()) {
        result[first].start_of_line = token.start_of_line;
        result[first].leading_space = token.leading_space;
      }
    } else {
      result.push_back(token);
    }
  }
  auto new_end = std::remove_if(result.begin(), result.end(), [](Token token) {
    return token.kind == TokenKind::placeholder;
  });
  result.erase(new_end, result.end());
  if (!result.empty()) {
    auto &first = result.front();
    first.leading_space = origin.leading_space;
    first.start_of_line = origin.start_of_line;
  }
  for (auto &token : result) {
    token.location = origin.location;
  }
  push(macro, into(std::move(result)));
}

auto ExpandCursor::advance_top_token() -> void {
  assert(top_token_.kind != TokenKind::eof);
  top_token_ = top_cursor_();
  while (top_token_.kind == TokenKind::eof && !token_stack_.empty()) {
    assert(macro_stack_.top()->expand_disable);
    macro_stack_.top()->expand_disable = false;
    top_token_ = token_stack_.top();
    top_cursor_ = std::move(cursor_stack_.top());

    macro_stack_.pop();
    token_stack_.pop();
    cursor_stack_.pop();
  }
}

auto ExpandCursor::push(Macro *macro, Cursor cursor) -> void {
  auto cache = top_token_;
  top_token_ = cursor();
  if (top_token_.kind == TokenKind::eof) {
    top_token_ = cache;
    return;
  }
  assert(!macro->expand_disable);
  macro->expand_disable = true;
  macro_stack_.push(macro);
  token_stack_.push(cache);
  cursor_stack_.push(std::move(top_cursor_));
  top_cursor_ = std::move(cursor);
}

} // namespace lzhcc