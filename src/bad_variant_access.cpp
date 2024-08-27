#include "additional.h"

bad_variant_access::bad_variant_access() : msg("bad_variant_access") {}

bad_variant_access::bad_variant_access(std::string message) : msg(std::move(message)) {}

bad_variant_access::bad_variant_access(const char* message) : msg(message) {}

const char* bad_variant_access::what() const noexcept {
  return msg.data();
}
