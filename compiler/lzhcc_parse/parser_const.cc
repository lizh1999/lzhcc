#include "lzhcc_parse.h"

namespace lzhcc {

static auto const_int(int64_t *value, Expr *expr) -> bool;
static auto const_unary_int(int64_t *value, UnaryExpr *expr) -> bool;
static auto const_binary_int(int64_t *value, BinaryExpr *expr) -> bool;
static auto const_condition_int(int64_t *value, ConditionExpr *expr) -> bool;

auto Parser::const_int(int64_t *value) -> bool {
  auto expr = condition();
  return lzhcc::const_int(value, expr);
}

auto const_int(int64_t *value, Expr *expr) -> bool {
  switch (expr->kind) {
  case ExperKind::integer:
    *value = cast<IntegerExpr>(expr)->value;
    return true;
  case ExperKind::unary:
    return const_unary_int(value, cast<UnaryExpr>(expr));
  case ExperKind::binary:
    return const_binary_int(value, cast<BinaryExpr>(expr));
  case ExperKind::condition:
    return const_condition_int(value, cast<ConditionExpr>(expr));
  case ExperKind::value:
  case ExperKind::call:
  case ExperKind::stmt:
  case ExperKind::member:
    return false;
  }
}

auto const_cast_int(int64_t *value, Type *type) -> bool {
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

auto const_unary_int(int64_t *value, UnaryExpr *expr) -> bool {
  switch (expr->kind) {
  case UnaryKind::negative:
    const_int(value, expr->operand);
    *value = -*value;
    return true;
  case UnaryKind::cast:
    const_int(value, expr->operand);
    return const_cast_int(value, expr->type);
  case UnaryKind::logical_not:
    const_int(value, expr->operand);
    *value = !*value;
    return true;
  case UnaryKind::bitwise_not:
    const_int(value, expr->operand);
    *value = ~*value;
    return true;
  case UnaryKind::refrence:
  case UnaryKind::deref:
    return false;
  }
}

auto const_binary_int(int64_t *value, BinaryExpr *expr) -> bool {
#define eval(op)                                                               \
  ({                                                                           \
    bool success = false;                                                      \
    int64_t lhs, rhs;                                                          \
    if (const_int(&lhs, expr->lhs) && const_int(&rhs, expr->rhs)) {            \
      *value = lhs op rhs;                                                     \
      success = true;                                                          \
    }                                                                          \
    success;                                                                   \
  })
  switch (expr->kind) {
  case BinaryKind::add:
    return eval(+);
  case BinaryKind::subtract:
    return eval(-);
  case BinaryKind::multiply:
    return eval(*);
  case BinaryKind::divide:
    return eval(/);
  case BinaryKind::modulo:
    return eval(%);
  case BinaryKind::less_than:
    return eval(<);
  case BinaryKind::less_equal:
    return eval(<=);
  case BinaryKind::equal:
    return eval(==);
  case BinaryKind::not_equal:
    return eval(!=);
  case BinaryKind::bitwise_or:
    return eval(|);
  case BinaryKind::bitwise_xor:
    return eval(^);
  case BinaryKind::bitwise_and:
    return eval(&);
  case BinaryKind::logical_and:
    return eval(&&);
  case BinaryKind::logical_or:
    return eval(||);
  case BinaryKind::shift_left:
    return eval(<<);
  case BinaryKind::shift_right:
    return eval(>>);
  case BinaryKind::comma:
    return const_int(value, expr->rhs);
  case BinaryKind::assign:
    return false;
  }
}

auto const_condition_int(int64_t *value, ConditionExpr *expr) -> bool {
  if (!const_int(value, expr->cond)) {
    return false;
  } else {
    return *value ? const_int(value, expr->then)
                  : const_int(value, expr->else_);
  }
}

} // namespace lzhcc