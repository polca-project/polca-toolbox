#include "polcascope.h"

int PolcaScope::_idCount = 0;

PolcaScope::PolcaScope() {
  clearParent();
}

PolcaScope::~PolcaScope() {

}

void PolcaScope::setId(int id) {
  _id = id;
}

void PolcaScope::setName(QString name) {
  _name = name;
}

void PolcaScope::setCodeLineStart(int line) {
  _codeLineStart = line;
}

void PolcaScope::setCodeLineEnd(int line) {
  _codeLineEnd = line;
}

int PolcaScope::id() {
  return _id;
}

QString PolcaScope::name() {
  return _name;
}

int PolcaScope::codeLineStart() {
  return _codeLineStart;
}

int PolcaScope::codeLineEnd() {
  return _codeLineEnd;
}

void PolcaScope::pragmasClear() {
  // This is supposed to destroy the objects
  _pragmas.clear();
}

void PolcaScope::pragmaAdd(PolcaPragma pragma) {
  _pragmas.push_back(pragma);
}

std::vector<PolcaPragma> PolcaScope::getPragmas() {
  return _pragmas;
}

int PolcaScope::parent() {
  return _parent;
}

void PolcaScope::setParent(int parent) {
  _parent = parent;
}

void PolcaScope::clearParent() {
  _parent = -1;
}

void PolcaScope::addNeighbourScope(ScopeNeighbourInfo neighbour) {
  _neighbours.push_back(neighbour);
}

void PolcaScope::clearNeighbours() {
  _neighbours.clear();
}

std::vector<ScopeNeighbourInfo> PolcaScope::neighbours() {
  return _neighbours;
}

void PolcaScope::addChildScope(int child) {
  _children.push_back(child);
}

void PolcaScope::clearChildren() {
  _children.clear();
}

void PolcaScope::setRoot(bool root) {
  _root = root;
}

bool PolcaScope::root() {
  return _root;
}

bool PolcaScope::isInChildren(int id) {
  for(int c : _children) {
    if(id == c)
      return true;
  }
  return false;
}

QString PolcaScope::pragmaTextAll() {
  QString s;

  for(PolcaPragma p : _pragmas) {
    if(s.isEmpty())
      s = p.text();
    else
      s = s + "\n" + p.text();
  }

  return s;
}

std::vector<int> PolcaScope::children() {
  return _children;
}

int PolcaScope::nPragmas() {
  return _pragmas.size();
}

int PolcaScope::idNext() {
  return _idCount;
}

void PolcaScope::idNextReset() {
  _idCount = 0;
}

void PolcaScope::idNextIncrease() {
  _idCount++;
}

bool PolcaScope::ainb(PolcaScope &a, PolcaScope &b) {
  if(b.codeLineStart() <  a.codeLineStart() &&
     b.codeLineEnd()   >= a.codeLineEnd()) {
    return true;
  }

  return false;
}

bool PolcaScope::automaticName() {
  if(_name != "")
    return false;

  QString name = "";
  for(PolcaPragma p : _pragmas) {
    // Find name by "def" pragma
    QStringList l = p.text().split(' ', QString::SkipEmptyParts);
    if(l[0] == "def") {
      name = l[1];
    }
  }
  if(name != "") {
    _name = name;
    return true;
  }
  return false;
}
