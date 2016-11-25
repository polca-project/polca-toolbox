#include <QDebug>
#include <QFile>

#include "pogadeprojectsourcefile.h"
#include "pogadeproject.h"


PogadeProjectSourceFile::PogadeProjectSourceFile(PogadeProject *project) {
  _project = project;
}

PogadeProjectSourceFile::PogadeProjectSourceFile(PogadeProject *project, QJsonObject data) {
  _project = project;
  _name = data.value("fileName").toString();
  _latestRev = (unsigned int) data.value("lastRevision").toInt();
  _id = (unsigned int) data.value("ID").toInt();

  QJsonArray _revs = data.value("revisions").toArray();
  QJsonArray _revsFather = data.value("revisionsFather").toArray();
  QJsonArray _revNames = data.value("revisionsNames").toArray();

  for(int i = 0; i< _revs.size(); ++i) {
    _revisions.append(_revs.at(i).toInt());
    _revisionsFather.append(_revsFather.at(i).toInt());
    _revisionsNames.append(_revNames.at(i).toString());
  }
}

PogadeProjectSourceFile::PogadeProjectSourceFile(PogadeProject *project, QString name, unsigned int id) {
  _project = project;
  _name = name;
  _latestRev = 0;
  _revisions.append(0);
  _revisionsFather.append(0);
  _revisionsNames.append("Original");
  _id = id;
}

PogadeProjectSourceFile::~PogadeProjectSourceFile() {

}

QString PogadeProjectSourceFile::name() {
  return _name;
}

QList<QString> PogadeProjectSourceFile::revisionsNames() {
  return _revisionsNames;
}

QList<unsigned int> PogadeProjectSourceFile::revisions() {
  return _revisions;
}

int PogadeProjectSourceFile::addRevision(int orgRev, QString revName) {
  if(!_project)
    return -1;

  _latestRev++;
  _project->generateCodeRevision(_name, orgRev, _latestRev);

  _revisions.append(_latestRev);
  _revisionsFather.append(orgRev);
  _revisionsNames.append(revName);

  return _latestRev;
}

void PogadeProjectSourceFile::removeRevision(unsigned int rev) {
    //TODO
}

unsigned int PogadeProjectSourceFile::id() {
  return _id;
}

QJsonValue PogadeProjectSourceFile::toJson() {
  QJsonObject _o;

  _o.insert("fileName", _name);
  _o.insert("ID", QJsonValue((int)_id));
  QJsonValue rev((int)_latestRev);
  _o.insert("lastRevision", rev);

  QJsonArray revs;
  QJsonArray revsFather;
  QJsonArray revsNames;
  for(int i = 0; i< _revisions.size(); ++i) {
    revs.append((int) _revisions.at(i));
    revsFather.append((int) _revisionsFather.at(i));
    revsNames.append(_revisionsNames.at(i));
  }

  _o.insert("revisions", revs);
  _o.insert("revisionsFather", revs);
  _o.insert("revisionsNames", revsNames);

  QJsonValue _v(_o);
  return _v;
}

bool PogadeProjectSourceFile::opened() {
  return _opened;
}

void PogadeProjectSourceFile::setOpen(bool open, QDockWidget *dock) {
  _opened = open;
  _dock = dock;
}

bool PogadeProjectSourceFile::visualized() {
  return _visualized;
}

void PogadeProjectSourceFile::setVisualized(bool visualized, QDockWidget* dock) {
  _visualized = visualized;
  _dockVisual = dock;
}

QString PogadeProjectSourceFile::code() {
  QFile file(_project->path() + "/src/" + _name);
  bool r = file.open(QFile::ReadOnly | QFile::Text);


  if(r)
    return QString::fromStdString(file.readAll().toStdString());
  else
    return "";
}

QString PogadeProjectSourceFile::codeRev(unsigned int rev) {
  QFile file(_project->path() + "/rev/" + _name + "_r" + QString::number(rev));
  bool r = file.open(QFile::ReadOnly | QFile::Text);

  if(r)
    return QString::fromStdString(file.readAll().toStdString());
  else
    return "";
}

unsigned int PogadeProjectSourceFile::nextRevision() {
  return _latestRev + 1;
}

QDockWidget *PogadeProjectSourceFile::getDock() {
    return _dock;
}

QDockWidget *PogadeProjectSourceFile::getDockVisual() {
    return _dockVisual;
}

QDockWidget *PogadeProjectSourceFile::getDockScopeTree() {
  return _dockScopeTree;
}

int PogadeProjectSourceFile::getRevInUse() {
  return _revInUse;
}

int PogadeProjectSourceFile::setRevInUse(unsigned int rev) {
  for(int i = 0; i< _revisions.size(); ++i) {
    if(_revisions.at(i) == rev) {
      _revInUse = rev;
      return 0;
    }
  }
  return -1;
}

int PogadeProjectSourceFile::saveCurrentRevision(QString text) {
  QFile file(_project->path() + "/rev/" + _name + "_r" + QString::number(_revInUse));
  file.open(QFile::WriteOnly | QFile::Text);
  file.write(text.toLatin1());

  return 0;
}

QString PogadeProjectSourceFile::getRevisionName(unsigned int rev) {
  for(int i = 0; i< _revisions.size(); ++i) {
    if(_revisions.at(i) == rev) {
      return _revisionsNames.at(i);
    }
  }
  return "";
}

void PogadeProjectSourceFile::addScope(PolcaScope ps) {
  _scopes.push_back(ps);
}

std::vector<PolcaScope> PogadeProjectSourceFile::getScopes() {
  return _scopes;
}

void PogadeProjectSourceFile::clearScopes() {
  //It should destroy the objects too
  _scopes.clear();
  _validScopes = false;
}

void PogadeProjectSourceFile::setValidScope(bool valid) {
  _validScopes = valid;
}

bool PogadeProjectSourceFile::validScope() {
  return _validScopes;
}

std::tuple<bool, PolcaScope, PolcaPragma> PogadeProjectSourceFile::findScopeAndPragmaFromLine(int line) {
  for(PolcaScope s : _scopes) {
    for(PolcaPragma p : s.getPragmas()) {
      if(line >= p.lineStart() && line <= p.lineEnd()) {
        return std::make_tuple(true, s, p);
      }
    }
  }

  PolcaScope s;
  PolcaPragma p;
  return std::make_tuple(false, s, p);
}

void PogadeProjectSourceFile::automaticNamesScopes() {
  for(PolcaScope &s : _scopes) {
    s.automaticName();

    //qDebug() << s.name() << " - " << s.id();
  }
}

void PogadeProjectSourceFile::processIOparamsScopes() {
  for(PolcaScope &s : _scopes) {
    s.procesIOparams();
  }
}

void PogadeProjectSourceFile::generateTreeScopes() {
  for(PolcaScope &s1 : _scopes) {
    std::vector<PolcaScope*> possibles;
    std::vector<PolcaScope*> children;
    for(PolcaScope &s2 : _scopes) {
      if(PolcaScope::ainb(s2, s1)) {
        possibles.push_back(&s2);
      }
    }

    for(PolcaScope *s2 : possibles) {
      bool inother = false;
      for(PolcaScope *s3 : possibles) {
        if(s2 != s3) {
          inother = inother || PolcaScope::ainb(*s2, *s3);
        }
      }
      if(!inother) {
        children.push_back(s2);
      }
    }

    for(PolcaScope *s2 : children) {
      std::vector<QString> _noVars;
      s1.addChildScope(s2->id(), s2->codeLineStart(), s2, _noVars);
      s2->setParent(&s1);
    }
  }
}

void PogadeProjectSourceFile::findRootScopes() {
  //Find out root scopes
  for(PolcaScope &s1 : _scopes) {
    bool isChild = false;
    for(PolcaScope &s2 : _scopes) {
      isChild = isChild || s2.isInChildren(s1.id());
    }

    s1.setRoot(!isChild);
  }
}

std::vector<ScopeChild> PogadeProjectSourceFile::generateOrderedRootChildren() {
  std::vector<ScopeChild> list;
  std::vector<PolcaScope*> _list;

  for(PolcaScope &s : _scopes) {
    if(s.root()) {
      _list.push_back(&s);
    }
  }

  //sort using a custom function object
  struct {
    bool operator()(PolcaScope* a, PolcaScope* b) {
      return a->codeLineStart() < b->codeLineStart();
    }
  } customLess;
  std::sort(_list.begin(), _list.end(), customLess);


  //Process data
  for(PolcaScope* _ps : _list) {
    ScopeChild sc;
    sc.cid    = _ps->id();
    sc.cline  = _ps->codeLineStart();
    sc.cscope = _ps;
    //sc.cscope->setParent(_ps);
    for(ParPos &pp : *(_ps->fParameters())) {
      ParChild pc;
      pc.parName = pp.var;
      pc.parPragmaName = pp.var;
      pc.input   = pp.input;
      pc.output  = pp.output;
      sc.vars.push_back(pc);
    }
    list.push_back(sc);
  }

  return list;
}

std::vector<PolcaScope*> PogadeProjectSourceFile::findScopes(QString name) {
  std::vector<PolcaScope*> v;

  for(PolcaScope &s : _scopes) {
    if(s.name() == name) {
      v.push_back(&s);
    }
  }

  return v;
}

PolcaScope *PogadeProjectSourceFile::findScope(QString name, int line) {
  if(line < 0) {
    for(PolcaScope &s : _scopes) {
      if(s.name() == name) {
        return &s;
      }
    }
  }
  else {
    for(PolcaScope &s : _scopes) {
      if(s.name() == name && s.isFunction() && s.callLine() == line) {
        return &s;
      }
    }
  }

  return nullptr;
}

PolcaScope PogadeProjectSourceFile::findScopeAndCopy(QString name) {
  for(PolcaScope s : _scopes) {
    if(s.name() == name) {
      return s;
    }
  }

  PolcaScope s;
  s.setId(-1);
  return s;
}

PolcaScope *PogadeProjectSourceFile::findScope(int id) {
  for(PolcaScope &s : _scopes) {
    if(s.id() == id) {
      return &s;
    }
  }

  return nullptr;
}

void PogadeProjectSourceFile::linkScopeChildren() {
  for(PolcaScope &s : _scopes) {
    PolcaScope::linkChildren(s.children());
  }
}

PolcaScope *PogadeProjectSourceFile::findScope(int startLine, int endLine) {
  for(PolcaScope &s : _scopes) {
    if(s.codeLineStart() == startLine && s.codeLineEnd() == endLine) {
      return &s;
    }
  }

  return nullptr;
}

std::vector<int> PogadeProjectSourceFile::rootScopes() {
  std::vector<int> rootIds;

  for(PolcaScope &s : _scopes) {
    if(s.root())
      rootIds.push_back(s.id());
  }

  return rootIds;
}

PolcaScope* PogadeProjectSourceFile::findScopeFromLine(int line) {
  std::vector<PolcaScope*> possibles;
  for(PolcaScope &s : _scopes) {
    if(s.codeLineStart() <= line && s.codeLineEnd() >= line) {
      possibles.push_back(&s);
    }
  }

  if(possibles.size() == 0) {
    return nullptr;
  }
  else {
    PolcaScope* selected = possibles[0];
    int size = selected->codeLineEnd() - selected->codeLineStart();
    for(unsigned int i=1; i<possibles.size(); i++) {
      int _size = possibles[i]->codeLineEnd() - possibles[i]->codeLineStart();
      if(_size < size) {
        size = size;
        selected = possibles[i];
      }
    }
    return selected;
  }
}

std::vector<PolcaTransformation*> PogadeProjectSourceFile::transformationsLine(int line) {
  std::vector<PolcaTransformation*> v;

  for(PolcaTransformation &pt : _transformations) {
    if(pt.lineStart() == line) {
      v.push_back(&pt);
    }
  }

  return v;
}

bool PogadeProjectSourceFile::scopeTransformations() {
  return _scopeTransformations;
}

void PogadeProjectSourceFile::setScopeTransformations(bool scopeTransformations, QDockWidget* dock) {
  _scopeTransformations = scopeTransformations;
  _dockTransformations = dock;
}

bool PogadeProjectSourceFile::scopeTree() {
  return _scopeTree;
}

void PogadeProjectSourceFile::setTransformationView(bool transformationView, QDockWidget* dock) {
  _transformationView = transformationView;
  _dockTransformationView = dock;
}

PolcaTransformation* PogadeProjectSourceFile::transformation(int id) {
  for(PolcaTransformation &pt : _transformations) {
    if(pt.transformationId() == id) {
      return &pt;
    }
  }
  return nullptr;
}

void PogadeProjectSourceFile::setScopeTree(bool scopeTree, QDockWidget* dock) {
  _scopeTree = scopeTree;
  _dockScopeTree = dock;
}

PogadeProject* PogadeProjectSourceFile::getProject() {
  return _project;
}

void PogadeProjectSourceFile::setMemoryShow(bool show) {
  _showMemory = show;
}

bool PogadeProjectSourceFile::memoryShow() {
  return _showMemory;
}

void PogadeProjectSourceFile::clearTransformations() {
  _transformations.clear();
}

void PogadeProjectSourceFile::addTransformation(PolcaTransformation pt) {
  _transformations.push_back(pt);
}

std::vector<PolcaTransformation> PogadeProjectSourceFile::getTransformations() {
  return _transformations;
}

std::vector<MemInfo> PogadeProjectSourceFile::rootMemory() {
  return _rootMemory;
}

void PogadeProjectSourceFile::addRootMemory(MemInfo mem) {
  _rootMemory.push_back(mem);
}

MemInfo *PogadeProjectSourceFile::findMemory(QString name) {
  for(MemInfo &m : _rootMemory) {
    if(m.name == name)
      return &m;
  }
  return nullptr;
}

void PogadeProjectSourceFile::setRootMemory() {
  for(PolcaScope &ps : _scopes) {
    if(ps.root() && ps.getType() == POLCA_MEMALLOC) {
      addRootMemory(ps.getMemoryInfo());
    }
  }
}

void PogadeProjectSourceFile::setMemoryInScopes() {
  for(PolcaScope &ps : _scopes) {
    for(ScopeChild &sc : ps.children()) {
      if(sc.cscope->getType() == POLCA_MEMALLOC) {
        ps.addChildMemory(sc.cscope->getMemoryInfo());
      }
    }
  }
}

std::vector<ASMCData> PogadeProjectSourceFile::getASMCData() {
  return _asmcData;
}

void PogadeProjectSourceFile::setASMCData(std::vector<ASMCData> data) {
  _asmcData = data;

  // Clear previous data
  for(PolcaScope &s : _scopes) {
    s.setASMWeightMine(0);
  }

  // Set new data
  for(ASMCData a : _asmcData) {
    if(a.name != "*") {
      std::vector<PolcaScope*> vs = findScopes(a.name);
      for(PolcaScope* ss : vs) {
        ss->setASMWeightMine(a.weight);
      }
    }
    else {
      //qDebug() << "Searching: " << a.lstart;
      PolcaScope *ps = this->findScopeFromLine(a.lstart);
      if(ps) {
        //qDebug() << "FOUND: " << ps->name();
      }
    }
  }
}

void PogadeProjectSourceFile::propagateMemoryInfo() {
  for(PolcaScope &s : _scopes) {
    if(s.root()) {
      // Set Memory Info for root scopes
      if(s.id() == 11) {
        qDebug() << "FOUND";
      }

      for(ParPos &pp : *(s.fParameters())) {
        MemInfo *mi = findMemory(pp.var);
        if(mi) {
          pp.numberElements = mi->elements;
          pp.sizeElement    = mi->eSize;
        }
      }

      // Propagate Memory in children of root scopes
      for(ScopeChild &sc : s.children()) {
        sc.cscope->setMemoryInfoFromParent();
      }
    }
  }
}
