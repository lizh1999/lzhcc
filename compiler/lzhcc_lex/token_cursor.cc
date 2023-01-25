#include "lzhcc_lex.h"
#include <cassert>
#include <filesystem>

namespace fs = std::filesystem;

namespace lzhcc {

TokenCursor::TokenCursor(CharCursor cursor, Context *context)
    : sb_defined(context->push_identifier("defined")),
      sb_define(context->push_identifier("define")),
      sb_undef(context->push_identifier("undef")),
      sb_include(context->push_identifier("include")),
      sb_if(context->push_identifier("if")),
      sb_ifdef(context->push_identifier("ifdef")),
      sb_ifndef(context->push_identifier("ifndef")),
      sb_else(context->push_identifier("else")),
      sb_elif(context->push_identifier("elif")),
      sb_endif(context->push_identifier("endif")),
      sb_error(context->push_identifier("error")), top_cursor_(cursor, context),
      context_(context), counter_(0) {
  top_token_ = top_cursor_();
  fs::path path = context_->filename(top_token_.location);
  file_.push(path);
  line_.push(0);
  context_->builtin_macro("__LINE__", [&](Token in) -> Token {
    int n = line_.top() + context_->line_number(in.location) + 1;
    return Token{
        .kind = TokenKind::numeric,
        .leading_space = in.leading_space,
        .start_of_line = in.start_of_line,
        .expand_disable = false,
        .location = in.location,
        .inner = context_->push_literal(std::to_string(n)),
    };
  });
  context_->builtin_macro("__FILE__", [&](Token in) -> Token {
    return Token{
        .kind = TokenKind::string,
        .leading_space = in.leading_space,
        .start_of_line = in.start_of_line,
        .expand_disable = true,
        .location = in.location,
        .inner = context_->push_literal("\"" + file_.top() + "\""),
    };
  });
  context_->builtin_macro("__COUNTER__", [&](Token in) -> Token {
    return Token{
        .kind = TokenKind::numeric,
        .leading_space = in.leading_space,
        .start_of_line = in.start_of_line,
        .expand_disable = false,
        .location = in.location,
        .inner = context_->push_literal(std::to_string(counter_++)),
    };
  });
}

auto TokenCursor::text_fn() -> std::function<Token()> {
  return std::bind(&TokenCursor::text, this);
}

auto TokenCursor::text() -> Token {
  if (top_token_.kind == TokenKind::eof) {
    top_token_.start_of_line = true;
    return top_token_;
  }
  auto token = top_token_;
  advance_top_token();
  if (!token.start_of_line || token.kind != TokenKind::hash) {
    return token;
  }
  if (top_token_.start_of_line) {
    return text();
  } else {
    auto token = top_token_;
    advance_top_token();
    if (token.kind != TokenKind::identifier) {
      context_->fatal(token.location, "");
    }
    auto directive = token.inner;
    if (directive == sb_include) {
      include_file(token.location);
      return text();
    }
    if (directive == sb_define) {
      define_macro();
      return text();
    }
    if (directive == sb_undef) {
      remove_macro();
      return text();
    }
    if (directive == sb_if) {
      cond_stack_.push(token.location);
      handle_if();
      return text();
    }
    if (directive == sb_ifdef) {
      cond_stack_.push(token.location);
      handle_ifdef();
      return text();
    }
    if (directive == sb_ifndef) {
      cond_stack_.push(token.location);
      handle_ifndef();
      return text();
    }
    if (directive == sb_else || directive == sb_elif) {
      if (cond_stack_.empty()) {
        context_->fatal(token.location, "");
      }
      cond_stack_.pop();
      skip_cond();
      return text();
    }
    if (directive == sb_endif) {
      if (cond_stack_.empty()) {
        context_->fatal(token.location, "");
      }
      cond_stack_.pop();
      return text();
    }
    if (directive == sb_error) {
      context_->fatal(token.location, "error");
    }
    assert(false);
  }
}

auto TokenCursor::advance_top_token() -> void {
  assert(top_token_.kind != TokenKind::eof);
  top_token_ = top_cursor_();
  while (top_token_.kind == TokenKind::eof && !token_stack_.empty()) {
    top_token_ = token_stack_.top();
    top_cursor_ = cursor_stack_.top();
    token_stack_.pop();
    cursor_stack_.pop();
    file_.pop();
    line_.pop();
  }
}

auto TokenCursor::skip_line() -> void {
  if (top_token_.start_of_line) {
    return;
  }
  while (!top_token_.start_of_line) {
    advance_top_token();
  }
}

auto TokenCursor::skip_cond() -> void {
  int depth = 1;
  while (top_token_.kind != TokenKind::eof) {
    if (!top_token_.start_of_line || top_token_.kind != TokenKind::hash) {
      advance_top_token();
      continue;
    }
    advance_top_token();
    if (top_token_.kind != TokenKind::identifier) {
      continue;
    }
    if (top_token_.inner == sb_if || top_token_.inner == sb_ifdef ||
        top_token_.inner == sb_ifndef) {
      depth++;
    }
    if (top_token_.inner == sb_endif) {
      advance_top_token();
      if (--depth == 0) {
        break;
      }
    }
  }
}

auto TokenCursor::include_file(int loc) -> void {
  std::vector<Token> tokens;
  {
    while (!top_token_.start_of_line) {
      tokens.push_back(top_token_);
      advance_top_token();
    }
    ExpandCursor cursor(std::move(tokens), context_);
    for (auto token = cursor(); token.kind != TokenKind::eof;) {
      tokens.push_back(token);
      token = cursor();
    }
  }
  if (tokens.empty()) {
    context_->fatal(loc, "");
  }

  std::string name;
  bool is_dquote = false;
  if (tokens.front().kind == TokenKind::string) {
    auto token = tokens.front();
    auto view = context_->storage(token.inner);
    name.assign(view.begin() + 1, view.end() - 1);
    is_dquote = true;
  } else {
    if (tokens.front().kind != TokenKind::less ||
        tokens.back().kind != TokenKind::greater) {
      context_->fatal(loc, "");
    }
    for (int i = 1; i + 1 < tokens.size(); i++) {
      name.append(context_->to_string(tokens[i]));
    }
  }

  fs::path path;
  if (name[0] == '/') {
    path = name;
    if (fs::exists(path)) {
      goto include;
    }
  } else {
    if (is_dquote) {
      path = context_->filename(loc);
      path = path.parent_path() / name;
      if (fs::exists(path)) {
        goto include;
      }
    }
    for (fs::path base : context_->arg.include_paths) {
      path = base / name;
      if (fs::exists(path)) {
        goto include;
      }
    }
  }
  context_->fatal(loc, "");
include:
  auto chars = context_->append_file(path);
  SourceCursor cursor(std::move(chars), context_);

  auto cache = top_token_;
  top_token_ = cursor();
  if (top_token_.kind == TokenKind::eof) {
    top_token_ = cache;
    return;
  }
  file_.push(path);
  line_.push(0);
  token_stack_.push(cache);
  cursor_stack_.push(std::move(top_cursor_));
  top_cursor_ = std::move(cursor);
}

auto TokenCursor::const_int() -> bool {
  std::vector<Token> tokens;
  while (!top_token_.start_of_line) {
    tokens.push_back(top_token_);
    advance_top_token();
  }
  auto new_end = tokens.begin();
  for (int i = 0; i < tokens.size(); i++) {
    if (tokens[i].kind != TokenKind::identifier ||
        tokens[i].inner != sb_defined) {
      *new_end++ = tokens[i];
      continue;
    }
    int loc = tokens[i++].location;
    bool has_paren = false;
    if (tokens[i].kind == TokenKind::open_paren) {
      has_paren = true;
      ++i;
    }
    if (tokens[i].kind != TokenKind::identifier) {
      context_->fatal(loc, "");
    }
    bool value = context_->find_macro(tokens[i].inner);
    if (has_paren) {
      if (tokens[++i].kind != TokenKind::close_paren) {
        context_->fatal(loc, "");
      }
    }
    *new_end++ = Token{
        .kind = TokenKind::numeric,
        .location = loc,
        .inner = context_->push_literal(value ? "1" : "0"),
    };
  }
  tokens.erase(new_end, tokens.end());
  ExpandCursor cursor(std::move(tokens), context_);

  do {
    auto token = cursor();
    if (token.kind == TokenKind::identifier) {
      token.kind = TokenKind::numeric;
      token.inner = context_->push_identifier("0");
    }
    tokens.push_back(token);
  } while (tokens.back().kind != TokenKind::eof);

  int64_t value;
  if (!lzhcc::const_int(tokens, *context_, &value)) {
    context_->fatal(cond_stack_.top(), "");
  }
  return value;
}

auto TokenCursor::handle_if() -> void {
  if (!const_int()) {
    if_group();
  }
}

auto TokenCursor::handle_ifdef() -> void {
  auto token = top_token_;
  advance_top_token();
  skip_line();
  if (token.kind != TokenKind::identifier) {
    context_->fatal(token.location, "");
  }
  if (!context_->find_macro(token.inner)) {
    if_group();
  }
}

auto TokenCursor::handle_ifndef() -> void {
  auto token = top_token_;
  advance_top_token();
  skip_line();
  if (token.kind != TokenKind::identifier) {
    context_->fatal(token.location, "");
  }
  if (context_->find_macro(token.inner)) {
    if_group();
  }
}

auto TokenCursor::if_group() -> void {
  int depth = 1;
  while (top_token_.kind != TokenKind::eof) {
    if (!top_token_.start_of_line || top_token_.kind != TokenKind::hash) {
      advance_top_token();
      continue;
    }
    advance_top_token();
    if (top_token_.kind != TokenKind::identifier) {
      continue;
    }
    if (top_token_.inner == sb_if || top_token_.inner == sb_ifdef ||
        top_token_.inner == sb_ifndef) {
      ++depth;
      advance_top_token();
      continue;
    }
    if (top_token_.inner == sb_else) {
      advance_top_token();
      if (depth == 1) {
        break;
      }
    } else if (top_token_.inner == sb_elif) {
      advance_top_token();
      if (const_int() && depth == 1) {
        break;
      }
    } else if (top_token_.inner == sb_endif) {
      advance_top_token();
      if (--depth == 0) {
        cond_stack_.pop();
        break;
      }
    }
  }
}

static auto join(ParamKind lhs, ParamKind rhs) {
  return static_cast<ParamKind>(static_cast<int>(lhs) | static_cast<int>(rhs));
}

auto TokenCursor::define_macro() -> void {
  if (top_token_.kind != TokenKind::identifier) {
    context_->fatal(top_token_.location, "");
  }
  bool is_variadic = false;
  int name = top_token_.inner;
  advance_top_token();
  if (top_token_.start_of_line || top_token_.leading_space ||
      top_token_.kind != TokenKind::open_paren) {
    std::vector<Token> replace;
    while (!top_token_.start_of_line) {
      replace.push_back(top_token_);
      advance_top_token();
    }
    context_->object_macro(name, std::move(replace));
  } else {
    auto param_map = collect_param(&is_variadic);
    std::vector<Token> replace;
    while (!top_token_.start_of_line) {
      if (top_token_.kind == TokenKind::identifier) {
        auto it = param_map.find(top_token_.inner);
        if (it != param_map.end()) {
          top_token_.kind = TokenKind::expand_arg;
          top_token_.inner = it->second;
        }
      }
      replace.push_back(top_token_);
      advance_top_token();
    }
    std::vector<ParamKind> param(param_map.size(), ParamKind::none);
    for (int i = 1; i < replace.size(); i++) {
      using enum TokenKind;
      auto &lhs = replace[i - 1], &rhs = replace[i];
      if (lhs.kind == TokenKind::hash) {
        if (rhs.kind != TokenKind::expand_arg) {
          context_->fatal(rhs.inner, "");
        }
        rhs.kind = TokenKind::raw_arg;
      } else if (lhs.kind == expand_arg && rhs.kind == hash_hash) {
        lhs.kind = TokenKind::raw_arg;
      } else if (lhs.kind == hash_hash && rhs.kind == expand_arg) {
        rhs.kind = TokenKind::raw_arg;
      }
    }
    for (auto &token : replace) {
      if (token.kind == TokenKind::expand_arg) {
        param[token.inner] = join(param[token.inner], ParamKind::expand);
      } else if (token.kind == TokenKind::raw_arg) {
        param[token.inner] = join(param[token.inner], ParamKind::raw);
      }
    }

    context_->function_macro(name, std::move(param), std::move(replace),
                             is_variadic);
  }
}

auto TokenCursor::collect_param(bool *is_variadic)
    -> std::unordered_map<int, int> {
  int open = top_token_.location;
  advance_top_token();
  if (top_token_.kind == TokenKind::close_paren) {
    advance_top_token();
    return {};
  }
  std::unordered_map<int, int> param_map;
  while (true) {
    if (top_token_.kind == TokenKind::identifier) {
      int index = param_map.size();
      int ident = top_token_.inner;
      if (param_map.contains(ident)) {
        context_->fatal(top_token_.location, "");
      }
      param_map.emplace(ident, index);
      advance_top_token();
    } else {
      *is_variadic = true;
      if (top_token_.kind != TokenKind::dotdotdot) {
        context_->fatal(top_token_.location, "");
      }
      int index = param_map.size();
      int ident = context_->push_identifier("__VA_ARGS__");
      param_map.emplace(ident, index);
      advance_top_token();
      if (top_token_.kind != TokenKind::close_paren) {
        context_->fatal(top_token_.location, "");
      }
      advance_top_token();
      break;
    }
    if (top_token_.kind == TokenKind::comma) {
      advance_top_token();
      continue;
    }
    if (top_token_.kind == TokenKind::close_paren) {
      advance_top_token();
      break;
    }
    context_->fatal(top_token_.location, "");
  }
  return param_map;
}

auto TokenCursor::remove_macro() -> void {
  if (top_token_.kind != TokenKind::identifier) {
    context_->fatal(top_token_.location, "");
  }
  int name = top_token_.inner;
  context_->remove_macro(name);
  skip_line();
}

} // namespace lzhcc