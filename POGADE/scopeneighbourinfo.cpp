#include "scopeneighbourinfo.h"

ScopeNeighbourInfo::ScopeNeighbourInfo() {

}

ScopeNeighbourInfo::~ScopeNeighbourInfo() {

}

std::unordered_set<int> ScopeNeighbourInfo::inNeighbours() {
  return _in;
}

void ScopeNeighbourInfo::addInNeighbours(int id) {
  _in.insert(id);
}

void ScopeNeighbourInfo::clearInNeighbours() {
  _in.clear();
}

std::unordered_set<int> ScopeNeighbourInfo::outNeighbours() {
  return _out;
}

void ScopeNeighbourInfo::addOutNeighbours(int id) {
  _out.insert(id);
}

void ScopeNeighbourInfo::clearOutNeighbours() {
  _out.clear();
}
