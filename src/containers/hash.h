#ifndef PAIRHASH_H
#define PAIRHASH_H

// for unordered_map < <uint32_t, uint32_t>, T >
namespace std {
template <>
struct hash<std::pair<uint32_t, uint32_t>> {
  inline uint64_t operator()(const pair<uint32_t, uint32_t>& k) const {
    return (uint64_t) k.first << 32 | k.second;
  }
};
}

#endif