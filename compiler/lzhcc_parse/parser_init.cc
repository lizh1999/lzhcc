#include "lzhcc_parse.h"
#include <cassert>
#include <cstdint>

namespace lzhcc {

auto Parser::array_designator() -> int {
  auto token = consume(TokenKind::open_bracket);
  int64_t x;
  if (!const_int(&x)) {
    context_->fatal(token->location, "");
  }
  consume(TokenKind::close_bracket);
  return x;
}

auto Parser::array_designation(Type *type, Init *&out) -> void {
  if (next_is(TokenKind::open_bracket)) {
    if (type->kind != TypeKind::array) {
      context_->fatal(position_->location, "");
    }
    auto array = cast<ArrayType>(type);
    if (out == nullptr) {
      out = context_->array_init({}, array->base);
    }
    auto out2 = cast<ArrayInit>(out);
    auto &children = out2->children;
    int i = array_designator();
    array_designation(array->base, children[i]);
    array_init(array, out2, i + 1);
  } else {
    consume(TokenKind::equal);
    if (next_is(TokenKind::open_brace)) {
      out = init(type);
    } else {
      init(type, out);
    }
  }
}

auto Parser::scalar_init() -> Init * {
  auto open = consume_if(TokenKind::open_brace);
  auto expr = assignment();
  if (open) {
    consume(TokenKind::close_brace);
  }
  return context_->scalar_init(expr);
}

auto Parser::array_init(ArrayType *array) -> Init * {
  std::map<int, Init *> children;
  switch (next_kind()) {
  case TokenKind::string: {
    auto base = array->base;
    if (base->kind != TypeKind::integer) {
      context_->fatal(position_->location, "");
    }
    IntegerType *type = 0;
    auto str = cook_string(type);
    while (next_is(TokenKind::string)) {
      str.append(cook_string(type));
    }
    int size;
    if (type->kind == IntegerKind::byte) {
      str.push_back(0);
      size = str.size();
    } else if (type->kind == IntegerKind::half) {
      str.push_back(0);
      str.push_back(0);
      size = str.size() / 2;
    } else {
      str.push_back(0);
      str.push_back(0);
      str.push_back(0);
      str.push_back(0);
      size = str.size() / 4;
    }
    if (array->length != -1 && array->length < size) {
      size = array->length;
    }
    switch (type->kind) {
    case IntegerKind::byte:
      for (int i = 0; i < size; i++) {
        auto expr = context_->integer(str[i]);
        children[i] = context_->scalar_init(expr);
      }
      break;
    case IntegerKind::half: {
      auto s = (uint16_t *)str.data();
      for (int i = 0; i < size; i++) {
        auto expr = context_->integer(s[i]);
        children[i] = context_->scalar_init(expr);
      }
      break;
    }
    case IntegerKind::word: {
      auto s = (uint32_t *)str.data();
      for (int i = 0; i < size; i++) {
        auto expr = context_->integer(s[i]);
        children[i] = context_->scalar_init(expr);
      }
      break;
    }
    case IntegerKind::dword:
      assert(false);
    }

    return context_->array_init(std::move(children), base);
  }
  case TokenKind::open_brace: {
    consume();
    bool first = true;
    int length = array->length == -1 ? INT_MAX : array->length;
    for (int i = 0; !consume_if(TokenKind::close_brace); i++) {
      if (!first) {
        consume(TokenKind::comma);
      }
      first = false;
      if (next_is(TokenKind::open_bracket)) {
        i = array_designator();
        array_designation(array->base, children[i]);
      } else if (!next_is(TokenKind::close_brace)) {
        children[i] = this->init(array->base);
      }
    }
    return context_->array_init(std::move(children), array->base);
  }
  default: {
    ArrayInit *out = nullptr;
    array_init(array, out);
    return out;
  }
  }
}

auto Parser::record_init(RecordType *record) -> Init * {
  if (next_is(TokenKind::open_brace)) {
    std::map<Member *, Init *> children;
    consume(TokenKind::open_brace);
    auto &members = record->members;
    int member_size = record->is_union ? 1 : members.size();
    for (int i = 0; !consume_if(TokenKind::close_brace); i++) {
      if (i == member_size) {
        context_->fatal(position_->location, "");
      }
      auto init = this->init(members[i].type);
      children[members.data() + i] = init;
      if (!consume_if(TokenKind::comma)) {
        consume(TokenKind::close_brace);
        break;
      }
    }
    return context_->record_init(std::move(children));
  } else {
    auto origin = position_;
    auto expr = assignment();
    if (expr->type == record) {
      return context_->scalar_init(expr);
    }
    position_ = origin;
    RecordInit *out = nullptr;
    record_init(record, out);
    return out;
  }
}

auto Parser::init(Type *type) -> Init * {
  switch (type->kind) {
  case TypeKind::array:
    return array_init(cast<ArrayType>(type));
  case TypeKind::boolean:
  case TypeKind::integer:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::floating:
    return scalar_init();
  case TypeKind::record:
    return record_init(cast<RecordType>(type));
  case TypeKind::kw_void:
    assert(false);
  }
}

auto Parser::array_init(ArrayType *array, ArrayInit *&out, int i) -> void {
  if (out == nullptr) {
    out = (ArrayInit *) context_->array_init({}, array->base);
  }
  auto &children = out->children;
  int length = array->length == -1 ? INT_MAX : array->length;
  for (; i < length; i++) {
    auto old_position = position_;
    if (i > 0) {
      consume_if(TokenKind::comma);
    }
    switch (next_kind()) {
    case TokenKind::open_bracket:
      position_ = old_position;
    case TokenKind::close_brace:
      return;
    default:
      init(array->base, children[i]);
    }
  }
}

auto Parser::record_init(RecordType *record, RecordInit *&out) -> void {
  if (out == nullptr) {
    out = (RecordInit *) context_->record_init({});
  }
  auto &children = out->children;
  auto &members = record->members;
  int member_size = record->is_union ? 1 : members.size();
  for (int i = 0; i < member_size; i++) {
    if (next_is(TokenKind::close_brace)) {
      break;
    }
    if (i != 0 && !consume_if(TokenKind::comma)) {
      break;
    }
    init(members[i].type, children[members.data() + i]);
  }
}

auto Parser::init(Type *type, Init *&init) -> void {
  switch (type->kind) {
  case TypeKind::array: {
    auto &array = reinterpret_cast<ArrayInit*&>(init);
    return array_init(cast<ArrayType>(type), array);
  }
  case TypeKind::boolean:
  case TypeKind::integer:
  case TypeKind::pointer:
  case TypeKind::function:
  case TypeKind::floating: {
    init = scalar_init();
    return;
  }
  case TypeKind::record: {
    auto record = reinterpret_cast<RecordInit*&>(init);
    record_init(cast<RecordType>(type), record);
  }
  case TypeKind::kw_void:
    assert(false);
  }
}

auto Parser::low_local_scalar(Expr *expr, ScalarInit *init, int loc)
    -> Expr * {
  return low_assign_op(context_, expr, init->expr, loc);
}

auto Parser::low_local_array(Expr *expr, ArrayInit *init, int loc) -> Expr * {
  Expr *result = nullptr;
  for (auto [i, child] : init->children) {
    auto index = context_->integer(i);
    auto base =
        low_deref_op(context_, low_add_op(context_, expr, index, loc), loc);
    auto rhs = this->low_local(base, child, loc);
    if (!result) {
      result = rhs;
    } else if (rhs) {
      result = context_->comma(rhs->type, result, rhs);
    }
  }
  return result;
}

auto Parser::low_local_record(Expr *expr, RecordInit *init, int loc)
    -> Expr * {
  Expr *result = nullptr;
  for (auto [member, child] : init->children) {
    auto base = context_->member(member->type, expr, member);
    auto rhs = this->low_local(base, child, loc);
    if (!result) {
      result = rhs;
    } else if (rhs) {
      result = context_->comma(rhs->type, result, rhs);
    }
  }
  return result;
}

auto Parser::low_local(Expr *expr, Init *init, int loc) -> Expr * {
  switch (init->kind) {
  case InitKind::array:
    return low_local_array(expr, cast<ArrayInit>(init), loc);
  case InitKind::scalar:
    return low_local_scalar(expr, cast<ScalarInit>(init), loc);
  case InitKind::record:
    return low_local_record(expr, cast<RecordInit>(init), loc);
  }
}

auto Parser::low_global_scalar(ScalarInit *init, std::span<uint8_t> out,
                                std::vector<Relocation> &relocations, int loc)
    -> void {
  auto write = [&](auto value) mutable {
    using T = decltype(value);
    *reinterpret_cast<T *>(&out[0]) = value;
  };
  if (init->expr->type->kind == TypeKind::floating) {
    double value;
    if (!const_float(init->expr, (double *)&value)) {
      context_->fatal(loc, "");
    }
    switch (out.size()) {
    case 4:
      return write(static_cast<float>(value));
    case 8:
      return write(static_cast<double>(value));
    default:
      assert(false);
    }
  } else {
    int64_t value;
    std::string_view *label = nullptr;

    if (!const_int(init->expr, &value, &label)) {
      context_->fatal(loc, "");
    }
    if (label) {
      assert(out.size() == 8);
      int64_t index = reinterpret_cast<int64_t>(&out[0]);
      relocations.push_back({index, *label, value});
      return;
    }

    switch (out.size()) {
    case 1:
      return write(static_cast<uint8_t>(value));
    case 2:
      return write(static_cast<uint16_t>(value));
    case 4:
      return write(static_cast<uint32_t>(value));
    case 8:
      return write(static_cast<uint64_t>(value));
    default:
      assert(false);
    }
  }
}

auto Parser::low_global_record(RecordInit *init, std::span<uint8_t> out,
                                std::vector<Relocation> &relocations, int loc)
    -> void {
  auto &children = init->children;
  for (auto [member, init] : init->children) {
    int start = member->offset;
    if (member->is_bitfield) {
      assert(init->kind == InitKind::scalar);
      auto expr = cast<ScalarInit>(init)->expr;
      int64_t value;
      if (!const_int(expr, &value, 0)) {
        context_->fatal(loc, "");
      }
      uint64_t old;
      uint8_t *buf = out.data() + start;

      int bytes = context_->size_of(member->type);
      switch (bytes) {
      case 1:
        old = *buf;
        break;
      case 2:
        old = *(uint16_t *)buf;
        break;
      case 4:
        old = *(uint32_t *)buf;
        break;
      case 8:
        old = *(uint64_t *)buf;
        break;
      }
      uint64_t mask = (1l << member->bit_width) - 1;
      old |= (value & mask) << member->bit_offset;
      switch (bytes) {
      case 1:
        *buf = old;
        break;
      case 2:
        *(uint16_t *)buf = old;
        break;
      case 4:
        *(uint32_t *)buf = old;
        break;
      case 8:
        *(uint64_t *)buf = old;
        break;
      }
    } else {
      int count = context_->size_of(member->type);
      low_global(init, out.subspan(start, count), relocations, loc);
    }
  }
}

auto Parser::low_global_array(ArrayInit *init, std::span<uint8_t> out,
                               std::vector<Relocation> &relocations, int loc)
    -> void {
  int size = context_->size_of(init->base);
  for (auto [i, child] : init->children) {
    low_global(child, out.subspan(i * size, size), relocations, loc);
  }
}

auto Parser::low_global(Init *init, std::span<uint8_t> out,
                         std::vector<Relocation> &relocations, int loc)
    -> void {
  switch (init->kind) {
  case InitKind::array:
    return low_global_array(cast<ArrayInit>(init), out, relocations, loc);
  case InitKind::scalar:
    return low_global_scalar(cast<ScalarInit>(init), out, relocations, loc);
  case InitKind::record:
    return low_global_record(cast<RecordInit>(init), out, relocations, loc);
  }
}

} // namespace lzhcc