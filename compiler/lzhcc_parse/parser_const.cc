#include "lzhcc_parse.h"

namespace lzhcc {

using LabelRef = std::string_view **;

static auto const_addr(Expr *, int64_t *, LabelRef) -> bool;

static auto const_int(Expr *, int64_t *, LabelRef) -> bool;
static auto const_value_int(ValueExpr *, int64_t *, LabelRef) -> bool;
static auto const_member_int(MemberExpr *, int64_t *, LabelRef) -> bool;
static auto const_unary_int(UnaryExpr *, int64_t *, LabelRef) -> bool;
static auto const_binary_int(BinaryExpr *, int64_t *, LabelRef) -> bool;
static auto const_condition_int(ConditionExpr *, int64_t *, LabelRef) -> bool;

auto Parser::const_int(int64_t *value) -> bool {
  auto expr = condition();
  return lzhcc::const_int(expr, value, nullptr);
}

auto Parser::const_int(Expr *expr, int64_t *value, LabelRef label) -> bool {
  return lzhcc::const_int(expr, value, label);
}

auto const_int(Expr *expr, int64_t *value,LabelRef label) -> bool {
  switch (expr->kind) {
  case ExperKind::integer:
    *value = cast<IntegerExpr>(expr)->value;
    return true;
  case ExperKind::unary:
    return const_unary_int(cast<UnaryExpr>(expr), value, label);
  case ExperKind::binary:
    return const_binary_int(cast<BinaryExpr>(expr), value, label);
  case ExperKind::condition:
    return const_condition_int(cast<ConditionExpr>(expr), value, label);
  case ExperKind::value:
    return const_value_int(cast<ValueExpr>(expr), value, label);
  case ExperKind::member:
    return const_member_int(cast<MemberExpr>(expr), value, label);
  case ExperKind::zero:
  case ExperKind::call:
  case ExperKind::stmt:
    return false;
  }
}

auto const_value_int(ValueExpr *expr, int64_t *offset, LabelRef label) -> bool {
  auto value = expr->value;
  if (value->kind != ValueKind::global) {
    return false;
  }
  auto type_kind = value->type->kind;
  if (!label || type_kind != TypeKind::array &&
      type_kind != TypeKind::function) {
    return false;
  }
  *offset = 0;
  *label = &cast<GValue>(value)->name;
  return true;
}

auto const_member_int(MemberExpr *expr, int64_t *offset, LabelRef label) -> bool {
  if (!label || expr->type->kind != TypeKind::array) {
    return false;
  } else if (const_addr(expr->record, offset, label)) {
    offset += expr->offset;
    return true;
  } else {
    return false;
  }
}

static auto const_cast_int(int64_t *value, Type *type) -> bool {
  if (type->kind == TypeKind::pointer) {
    return true;
  } else if (type->kind != TypeKind::integer) {
    return false;
  }
  switch (cast<IntegerType>(type)->kind) {
  case IntegerKind::byte:
    *value = static_cast<int8_t>(*value);
    return true;
  case IntegerKind::half:
    *value = static_cast<int16_t>(*value);
    return true;
  case IntegerKind::word:
    *value = static_cast<int32_t>(*value);
    return true;
  case IntegerKind::dword:
    *value = static_cast<int64_t>(*value);
    return true;
  }
}

auto const_unary_int(UnaryExpr *expr, int64_t *value, LabelRef label) -> bool {
  switch (expr->kind) {
  case UnaryKind::negative:
    const_int(expr->operand, value, nullptr);
    *value = -*value;
    return true;
  case UnaryKind::cast:
    const_int(expr->operand, value, label);
    return const_cast_int(value, expr->type);
  case UnaryKind::logical_not:
    const_int(expr->operand, value, nullptr);
    *value = !*value;
    return true;
  case UnaryKind::bitwise_not:
    const_int(expr->operand, value, nullptr);
    *value = ~*value;
    return true;
  case UnaryKind::refrence:
    return label ? const_addr(expr->operand, value, label) : false;
  case UnaryKind::deref:
    return false;
  }
}

auto const_binary_int(BinaryExpr *expr, int64_t *value, LabelRef label)
    -> bool {
#define eval(op, x)                                                            \
  ({                                                                           \
    bool success = false;                                                      \
    int64_t lhs, rhs;                                                          \
    if (const_int(expr->lhs, &lhs, x) && const_int(expr->rhs, &rhs, 0)) {      \
      *value = lhs op rhs;                                                     \
      success = true;                                                          \
    }                                                                          \
    success;                                                                   \
  })
  switch (expr->kind) {
  case BinaryKind::add:
    return eval(+, label);
  case BinaryKind::subtract:
    return eval(-, label);
  case BinaryKind::multiply:
    return eval(*, nullptr);
  case BinaryKind::divide:
    return eval(/, nullptr);
  case BinaryKind::modulo:
    return eval(%, nullptr);
  case BinaryKind::less_than:
    return eval(<, nullptr);
  case BinaryKind::less_equal:
    return eval(<=, nullptr);
  case BinaryKind::equal:
    return eval(==, nullptr);
  case BinaryKind::not_equal:
    return eval(!=, nullptr);
  case BinaryKind::bitwise_or:
    return eval(|, nullptr);
  case BinaryKind::bitwise_xor:
    return eval(^, nullptr);
  case BinaryKind::bitwise_and:
    return eval(&, nullptr);
  case BinaryKind::logical_and:
    return eval(&&, nullptr);
  case BinaryKind::logical_or:
    return eval(||, nullptr);
  case BinaryKind::shift_left:
    return eval(<<, nullptr);
  case BinaryKind::shift_right:
    return eval(>>, nullptr);
  case BinaryKind::comma:
    return const_int(expr->rhs, value, label);
  case BinaryKind::assign:
    return false;
  }
}

auto const_condition_int(ConditionExpr *expr, int64_t *value, LabelRef label)
    -> bool {
  if (!const_int(expr->cond, value, nullptr)) {
    return false;
  } else {
    return *value ? const_int(expr->then, value, label)
                  : const_int(expr->else_, value, label);
  }
}

auto const_addr(Expr *expr, int64_t *offset, LabelRef label) -> bool {
  if (expr->kind == ExperKind::value) {
    auto value = cast<ValueExpr>(expr)->value;
    if (value->kind != ValueKind::global) {
      return false;
    }
    *label = &cast<GValue>(value)->name;
    *offset = 0;
    return true;
  }
  if (expr->kind == ExperKind::unary) {
    auto unary = cast<UnaryExpr>(expr);
    if (unary->kind == UnaryKind::deref) {
      return const_int(unary->operand, offset, label);
    } else {
      return false;
    }
  }

  if (expr->kind == ExperKind::member) {
    auto member = cast<MemberExpr>(expr);
    if (const_addr(member->record, offset, label)) {
      *offset += member->offset;
      return true;
    } else {
      return false;
    }
  }

  return false;
}

} // namespace lzhcc