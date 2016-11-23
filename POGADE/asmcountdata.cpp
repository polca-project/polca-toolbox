#include "asmcountdata.h"

std::vector<ASMCData> getScopes(QJsonArray scopes, int *nextId, std::vector<int> *child) {
  std::vector<ASMCData> v;
  std::vector<int> myChildren;

  for(QJsonValue s : scopes) {
    ASMCData d;
    d.id = *nextId;
    *nextId = d.id + 1;
    if(child){
      child->push_back(d.id);
    }
    d.name = s.toObject().value("name").toString();
    d.weight = s.toObject().value("weight").toInt();
    d.lstart = s.toObject().value("lstart").toInt();
    d.lend = s.toObject().value("lend").toInt();

    std::vector<ASMCData> _v = getScopes(s.toObject().value("scopes").toArray(), nextId, &myChildren);
    d.scopes = myChildren;
    for(ASMCData _ve : _v) {
      v.push_back(_ve);
    }
    v.push_back(d);
  }

  return v;
}
