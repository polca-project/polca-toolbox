#include "polcascope.h"
#include <QDebug>

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

/*
void PolcaScope::addNeighbourScope(ScopeNeighbourInfo neighbour) {
  _neighbours.push_back(neighbour);
}

void PolcaScope::clearNeighbours() {
  _neighbours.clear();
}

std::vector<ScopeNeighbourInfo> PolcaScope::neighbours() {
  return _neighbours;
}
*/


void PolcaScope::addChildScope(int child, int line, PolcaScope* pscope,
                               std::vector<QString> var) {
  ScopeChild _sc;
  _sc.cid    = child;
  _sc.cline  = line;
  _sc.cscope = pscope;

  /*
  qDebug() << "AAA====================";
  qDebug() << "Father: " << this->name();
  qDebug() << "child:  " << pscope->name();
  for(unsigned int i = 0; i< var.size(); i++) {
    qDebug() << i << " " << var[i];
  }
  qDebug() << "BBB====================";
  */

  for(unsigned int i = 0; i< var.size(); i++) {
    bool in, out;
    ParChild par;
    QString pragmaName;
    pscope->getParPragmaFunc(i, &pragmaName, &in, &out);
    par.input   = in;
    par.output  = out;
    par.parName = var[i];
    par.parPragmaName = pragmaName;
    _sc.vars.push_back(par);
  }
  _children.push_back(_sc);


  ///////////////////////
  ///////////////////////
  /*
  qDebug() << this->name();
  qDebug() << pscope->name() << " - " << line;
  for(unsigned int i = 0; i<_sc.vars.size(); i++) {
    qDebug() << _sc.vars[i].parName << "  - I:" << _sc.vars[i].input << "  - O:" << _sc.vars[i].output;
  }
  */
  ///////////////////////
  ///////////////////////

  // sort using a custom function object
  struct {
    bool operator()(ScopeChild a, ScopeChild b) {
      return a.cline < b.cline;
    }
  } customLess;

  std::sort(_children.begin(), _children.end(), customLess);
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
  for(ScopeChild c : _children) {
    if(id == c.cid)
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

std::vector<ScopeChild> PolcaScope::children() {
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

void PolcaScope::setIsFunction(bool function) {
  _isFunction = function;
}

bool PolcaScope::isFunction() {
  return _isFunction;
}

void PolcaScope::clearfParameters() {
  _parPosC.clear();
}

void PolcaScope::addfParameter(ParPos p) {
  _parPosC.push_back(p);
}

std::vector<ParPos> PolcaScope::fParameters() {
  return _parPosC;
}

void PolcaScope::automaticType() {
  int type = 0;
  for(PolcaPragma p : _pragmas) {
    QStringList l = p.text().split(' ', QString::SkipEmptyParts);
    type = stringToType(l[0]);
    if(type)
      break;
  }
  _type = type;
}

int PolcaScope::getType() {
  return _type;
}

QString PolcaScope::typeToString(int type) {
  switch(type) {
    case POLCA_MAP:
      return "map";
      break;
    case POLCA_ITN:
      return "itn";
      break;
    case POLCA_ZIPWITH:
      return "zipwith";
      break;
    case POLCA_FOLDL:
      return "foldl";
      break;
    case POLCA_MEMALLOC:
      return "memalloc";
      break;
    case POLCA_MEMFREE:
      return "memfree";
      break;
    case POLCA_NONE:
      return "";
      break;
    default:
      return "";
  }
}

int PolcaScope::stringToType(QString s) {
  QString _s = s.toLower();
  if(_s == "map") {
    return POLCA_MAP;
  }
  else if(_s == "itn") {
    return POLCA_ITN;
  }
  else if(_s == "zipwith") {
    return POLCA_ZIPWITH;
  }
  else if(_s == "foldl") {
    return POLCA_FOLDL;
  }
  else if(_s == "memalloc") {
    return POLCA_MEMALLOC;
  }
  else if(_s == "memfree") {
    return POLCA_MEMFREE;
  }
  else {
    return POLCA_NONE;
  }
}

void PolcaScope::setIOPar(ParPos*p, int type) {
  p->input  = false;
  p->output = false;

  for(unsigned int i=0; i<p->posP.size(); i++) {
    int pos = p->posP[i];
    switch(type) {
      case POLCA_MAP:
        if(pos == 1) {
          p->input  = true;
        } else if(pos == 2) {
          p->output = true;
        } else {
        }
        break;
      case POLCA_ITN:
      case POLCA_ZIPWITH:
      case POLCA_FOLDL:
        if(pos == 1) {
          p->input  = true;
        } else if(pos == 2) {
          p->input = true;
        } else if(pos == 3) {
          p->output = true;
        } else {
        }
        break;
      default:
        //p->input  = false;
        //p->output = false;
        break;
    }
  }
}

void PolcaScope::procesIOparams() {
  for(ParPos &p : _parPosC) {
    setIOPar(&p, this->_type);
  }
}

void PolcaScope::printIO(std::vector<ParChild> varsC) {
  qDebug() << "========== printIO ===========";
  for(ParPos p : _parPosC) {
    qDebug() << p.var << "  - C: " << varsC[p.posC-1].parName;
    qDebug() << "  I: " << p.input << " - O: " << p.output;
  }
}


void PolcaScope::getParPragmaFunc(int pos, QString *pragmaName, bool *in, bool *out) {
  *in  = false;
  *out = false;
  *pragmaName = "";
  for(unsigned int i = 0; i< _parPosC.size(); i++) {
    if(_parPosC[i].posC == pos) {
      *in  = _parPosC[i].input;
      *out = _parPosC[i].output;
      *pragmaName = _parPosC[i].var;
      return;
    }
  }
}

bool PolcaScope::isParInput(ScopeChild& sc, QString par) {
  for(ParChild pc : sc.vars) {
    if(pc.parPragmaName == par) {
      return pc.input;
    }
  }
  return false;
}

bool PolcaScope::isParOutput(ScopeChild& sc, QString par) {
  for(ParChild pc : sc.vars) {
    //qDebug() << "----------------";
    //qDebug() << par << " ---- " << pc.parName << " (" << pc.parPragmaName << ")";
    if(pc.parPragmaName == par) {
      return pc.output;
    }
  }
  return false;
}

void PolcaScope::linkChildren() {
  // TODO: link children

  if(_children.size() > 1) {
    for(unsigned int i=0; i<_children.size(); i++) {
      ScopeChild c1  = _children[i];
      PolcaScope *sc = c1.cscope;

      for(ParPos p : sc->fParameters()) {
        if(p.output) {
          for(unsigned int j=i+1; j<_children.size(); j++) {
            ScopeChild c2  = _children[j];
            bool isInput  = isParInput(c2, p.var);
            bool isOutput = isParOutput(c2, p.var);

            if(isInput) {
              c2.cscope->_neighbours.addInNeighbours(c1.cid);
              c1.cscope->_neighbours.addOutNeighbours(c2.cid);
              //qDebug() << "Connect: " << c1.cid << " to " << c2.cid;
            }
            if(isOutput) {
              // new output, break
              j = _children.size();
            }

          }
        }
      }


    }
  }
}


ScopeNeighbourInfo PolcaScope::neighbours() {
  return _neighbours;
}
