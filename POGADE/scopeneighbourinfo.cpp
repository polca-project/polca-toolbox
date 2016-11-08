#include "scopeneighbourinfo.h"

ScopeNeighbourInfo::ScopeNeighbourInfo() {

}

ScopeNeighbourInfo::~ScopeNeighbourInfo() {

}

void ScopeNeighbourInfo::setId(int id) {
  _id = id;
}

int ScopeNeighbourInfo::id() {
  return _id;
}

std::vector<int> ScopeNeighbourInfo::inChildren() {
  return _in;
}

void ScopeNeighbourInfo::addInChildren(int id) {
  _in.push_back(id);
}

void ScopeNeighbourInfo::clearInChildren() {
  _in.clear();
}

std::vector<int> ScopeNeighbourInfo::outChildren() {
  return _out;
}

void ScopeNeighbourInfo::addOutChildren(int id) {
  _out.push_back(id);
}

void ScopeNeighbourInfo::clearOutChildren() {
  _out.clear();
}
