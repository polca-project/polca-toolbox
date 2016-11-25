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

void PolcaScope::addIOInfo(QString var, int type) {
  ParPos *_p = nullptr;
  for(ParPos p : _parPosC) {
    if(p.var == var) {
      _p = &p;
    }
  }

  if(_p) {
    if(type == PARIN) {
      _p->input = true;
    }
    else if(type == PAROUT) {
      _p->output = true;
    }
  }
  else {
    ParPos p;
    p.var  = var;
    p.posC = this->_parPosC.size() + 1;
    p.posP.push_back(p.posC);
    p.input  = false;
    p.output = false;


    if(type == PARIN) {
      p.input = true;
    }
    else if(type == PAROUT) {
      p.output = true;
    }

    this->_parPosC.push_back(p);
  }

}

void PolcaScope::setIOFromPragma(PolcaPragma pragma) {
  QStringList l = pragma.text().split(' ', QString::SkipEmptyParts);
  if(l[0] == "input") {
    for(int i=1; i<l.size(); i++) {
      this->addIOInfo(l[i], PARIN);
    }
  }
  else if(l[0] == "output") {
    for(int i=1; i<l.size(); i++) {
      this->addIOInfo(l[i], PAROUT);
    }
  }
  else if(l[0] == "inout") {
    for(int i=1; i<l.size(); i++) {
      this->addIOInfo(l[i], PARIN);
      this->addIOInfo(l[i], PAROUT);
    }
  }
}

void PolcaScope::pragmaAdd(PolcaPragma pragma) {
  _pragmas.push_back(pragma);
  this->setIOFromPragma(pragma);
}

std::vector<PolcaPragma> PolcaScope::getPragmas() {
  return _pragmas;
}

int PolcaScope::parentId() {
  if(_parent) {
    return _parent->id();
  }

  return -1;
}

PolcaScope* PolcaScope::parent() {
  return _parent;
}

void PolcaScope::setParent(PolcaScope* parent) {
  _parent = parent;
}

void PolcaScope::clearParent() {
  _parent = nullptr;
}

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
    pscope->getParPragmaFunc(i+1, &pragmaName, &in, &out);
    par.input   = in;
    par.output  = out;
    par.parName = var[i];
    par.parPragmaName = pragmaName;
    _sc.vars.push_back(par);
  }
  _sc.cscope->setParent(this);
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

std::vector<ParPos>* PolcaScope::fParameters() {
  return &_parPosC;
}

std::vector<QStringList> PolcaScope::splitParameters(QString params) {
  std::vector<QStringList> v;
  QStringList l = params.split(' ', QString::SkipEmptyParts);
  QStringList *_l;
  bool inZip = false;

  for(int i=0; i<l.size(); i++) {
    if(i==0) {
      _l = new QStringList();
      _l->append(l[i]);
      v.push_back(*_l);
      delete _l;
    }
    else {
      if(inZip) {
        QString s1 = l[i];
        if(s1.endsWith(")")) {
          QString s2 = s1.mid(0, s1.size()-1);
          _l->append(s2);
          v.push_back(*_l);
          delete _l;
          inZip = false;
        }
        else {
          QString s2 = s1.mid(0, s1.size()-1);
          _l->append(s2);
        }
      }
      else {
        QString s1 = l[i];
        if(s1.startsWith("zip(")) {
          _l = new QStringList();

          QString s2 = s1.mid(4, s1.size()-5);
          _l->append(s2);
          inZip = true;
        }
        else {
          _l = new QStringList();
          _l->append(l[i]);
          v.push_back(*_l);
          delete _l;
        }
      }
    }
  }

  return v;
}

void PolcaScope::automaticType() {
  int type = 0;
  for(PolcaPragma p : _pragmas) {
    std::vector<QStringList> l = splitParameters(p.text());
    QStringList _l = l[0];
    type = stringToType(_l[0]);
    if(type) {
      this->_parPosC = processPragmaIO(type, l);
      break;
    }
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

ParPos* PolcaScope::getParPosWithName(std::vector<ParPos>* list, QString varName) {
  for(ParPos& p : *list) {
    if(p.var == varName) {
      return &p;
    }
  }

  return nullptr;
}

std::vector<ParPos> PolcaScope::processPragmaIO(int type, std::vector<QStringList> l) {
  std::vector<ParPos> _pars;
  ParPos* p;
  ParPos* _p;
  QStringList _l1;
  QStringList _l2;

  switch(type) {
    case POLCA_MAP:
      for(QString _l : l[2]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(1);
          p->input = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(1);
          _p->input  = true;
          _p->output = false;
          _pars.push_back(*_p);
        }
      }

      for(QString _l : l[3]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(2);
          p->output = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(2);
          _p->input  = false;
          _p->output = true;
          _pars.push_back(*_p);
        }
      }
      break;
    case POLCA_ITN:
      for(QString _l : l[2]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(1);
          p->input = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(1);
          _p->input  = true;
          _p->output = false;
          _pars.push_back(*_p);
        }
      }

      for(QString _l : l[3]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(2);
          p->input = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(2);
          _p->input  = true;
          _p->output = false;
          _repeat = _l;
          _pars.push_back(*_p);
        }
      }

      for(QString _l : l[4]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(2);
          p->output = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(3);
          _p->input  = false;
          _p->output = true;
          _pars.push_back(*_p);
        }
      }
      break;
    case POLCA_ZIPWITH:
    case POLCA_FOLDL:
      for(QString _l : l[2]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(1);
          p->input = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(1);
          _p->input  = true;
          _p->output = false;
          _pars.push_back(*_p);
        }
      }

      for(QString _l : l[3]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(2);
          p->input = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(2);
          _p->input  = true;
          _p->output = false;
          _pars.push_back(*_p);
        }
      }

      for(QString _l : l[4]) {
        p = getParPosWithName(&_pars, _l);
        if(p) {
          p->posP.push_back(2);
          p->output = true;
        } else {
          _p = new ParPos;
          _p->var    = _l;
          _p->posC   = _pars.size()+1;
          _p->posP.push_back(3);
          _p->input  = false;
          _p->output = true;
          _pars.push_back(*_p);
        }
      }
      break;
    case POLCA_MEMALLOC:
      // TODO: ADD memory size
      _l1 = l[3];
      _p = new ParPos;
      _p->var    = _l1[0];
      _p->posC   = _pars.size()+1;
      _p->posP.push_back(1);
      _p->input  = false;
      _p->output = true;
      _p->numberElements = l[2][0];
      _p->sizeElement    = l[1][0];
      _mem.name     = l[3][0];
      _mem.elements = l[2][0];
      _mem.eSize    = l[1][0];
      _pars.push_back(*_p);
      break;
    case POLCA_MEMFREE:
      _l2 = l[1];
      _p = new ParPos;
      _p->var    = _l2[0];
      _p->posC   = _pars.size()+1;
      _p->posP.push_back(1);
      _p->input  = true;
      _p->output = true;
      _pars.push_back(*_p);
      break;
    default:
      //p->input  = false;
      //p->output = false;
      break;
  }

  return _pars;
}

void PolcaScope::setIOPar(ParPos*p, int type) {
  //p->input  = false;
  //p->output = false;

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
        if(pos == 1) {
          p->input  = true;
        } else if(pos == 2) {
          p->input = true;
        } else if(pos == 3) {
          p->output = true;
        } else {
        }
        break;
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
      case POLCA_MEMALLOC:
        if(pos == 1) {
          p->output = true;
        }
        break;
      case POLCA_MEMFREE:
        if(pos == 1) {
          p->input  = true;
          p->output = true;
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
  //qDebug() << this->_name << " T: "<< this->typeToString(_type) << " - Params: " << _parPosC.size();

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
  bool result = false;
  for(ParChild pc : sc.vars) {
    if(pc.parName == par) {
      result = result || pc.input;
    }
  }
  return result;
}

bool PolcaScope::isParOutput(ScopeChild& sc, QString par) {
  bool result = false;
  for(ParChild pc : sc.vars) {
    if(pc.parName == par) {
      result = result ||  pc.output;
    }
  }
  return result;
}

void PolcaScope::linkChildren(std::vector<ScopeChild> sChildren) {
  // TODO: link children

  if(sChildren.size() > 1) {
    for(unsigned int i=0; i<sChildren.size(); i++) {
      ScopeChild c1  = sChildren[i];
      //PolcaScope *sc = c1.cscope;

      for(ParChild p : c1.vars) {
        //qDebug() << "-------------------------------";
        //qDebug() << "F: " << c1.cid << " V: " << p.parName;
        if(p.output) {
          for(unsigned int j=i+1; j<sChildren.size(); j++) {
            ScopeChild c2  = sChildren[j];
            bool isInput  = isParInput(c2, p.parName);
            bool isOutput = isParOutput(c2, p.parName);

            //qDebug() << "F: " << c2.cid << " I: " << isInput << " - O: "<< isOutput;

            if(isInput) {
              c2.cscope->_neighbours.addInNeighbours(c1.cid);
              c1.cscope->_neighbours.addOutNeighbours(c2.cid);
              //qDebug() << "Connect: " << c1.cid << " to " << c2.cid;
            }
            if(isOutput) {
              // new output, break
              j = sChildren.size();
            }

          }
        }
      }
    }
  }
}

void PolcaScope::setCallLine(int line) {
  _callLine = line;
}

int PolcaScope::callLine() {
  return _callLine;
}

ScopeNeighbourInfo PolcaScope::neighbours() {
  return _neighbours;
}

void PolcaScope::setASMWeightMine(int w) {
  _ASMWeightMine = w;
}

int PolcaScope::getASMWeightMine() {
  if(_ASMWeightMine) {
    return _ASMWeightMine;
  }
  else {
    switch(_type) {
      case POLCA_MEMALLOC:
        return 9;
        break;
      case POLCA_MEMFREE:
        return 4;
        break;
    }

    return 3;
  }
}

QString PolcaScope::getASMWeightTotal() {
  /*
  if(this->children().empty()) {
    _ASMWeightTotal = QString::number(getASMWeightMine());
  }
  else {
  */
    switch(_type) {
      case POLCA_ITN:
        _ASMWeightTotal = QString::number(getASMWeightMine());
        _ASMWeightTotal += " + " + _repeat + "*(";

        for(ScopeChild sc : _children) {
          _ASMWeightTotal += sc.cscope->getASMWeightTotal();
        }

        _ASMWeightTotal += ")";
        break;
      case POLCA_MAP:
      case POLCA_ZIPWITH:
      case POLCA_FOLDL:
        if(this->_repeat != "") {
          _ASMWeightTotal = QString::number(getASMWeightMine());
          _ASMWeightTotal += " + " + _repeat + "*(";
        }
        else {
        _ASMWeightTotal = QString::number(getASMWeightMine()) + " + N*(";
        }

        for(ScopeChild sc : _children) {
          _ASMWeightTotal += sc.cscope->getASMWeightTotal();
        }

        _ASMWeightTotal += ")";
        break;
      case POLCA_MEMALLOC:
        _ASMWeightTotal = QString::number(getASMWeightMine()) + " + OS_CALL_MALLOC";
        break;
      case POLCA_MEMFREE:
        _ASMWeightTotal = QString::number(getASMWeightMine()) + " + OS_CALL_FREE";
        break;
      default:
        _ASMWeightTotal = QString::number(getASMWeightMine()) + " + ";
        for(ScopeChild sc : _children) {
          QString nv = sc.cscope->getASMWeightTotal();
          if(nv != "0") {
            _ASMWeightTotal += nv;
            _ASMWeightTotal += " + ";
          }
        }
        _ASMWeightTotal.chop(3);
        break;
    }

  return _ASMWeightTotal;
}

MemInfo PolcaScope::getMemoryInfo() {
  return _mem;
}

void PolcaScope::addChildMemory(MemInfo mem) {
  _childMem.push_back(mem);
}

void PolcaScope::clearChildMemory() {
  _childMem.clear();
}

std::vector<MemInfo> PolcaScope::getChildMem() {
  return _childMem;
}

MemInfo *PolcaScope::findMemName(QString name) {
  for(MemInfo &m :_childMem ) {
    if(m.name == name)
      return &m;
  }
  return nullptr;
}

void PolcaScope::setMemoryInfoFromParent() {
  // TODO:
  PolcaScope* parent = this->_parent;
  if(!_parent) {
    return;
  }

  std::vector<ParPos>* mem = parent->fParameters();
  for(ParPos &pp : _parPosC) {
    ParPos *matchedpp = getParPosWithName(mem, pp.var);
    if(matchedpp) {
      pp.numberElements = matchedpp->numberElements;
      pp.sizeElement = matchedpp->sizeElement;
    }
  }

  std::vector<ParPos>* memChild = parent->fParameters();
  for(ScopeChild &sc : _children) {
    for(ParChild &pc : sc.vars) {
      ParPos *matchedpp = getParPosWithName(memChild, pc.parName);
      if(matchedpp) {
        pc.numberElements = matchedpp->numberElements;
        pc.sizeElement = matchedpp->sizeElement;
      }
    }

    sc.cscope->matchMemInfoParPragma(&(sc.vars));
    sc.cscope->setMemoryInfoFromParent();
    sc.cscope->analyzeRepetitions();

  }
}

void PolcaScope::analyzeRepetitions() {
  switch(_type) {
    case POLCA_ITN:
      // this one should be already solved
      break;
    case POLCA_MAP:
      for(ParPos &pp : _parPosC) {
        for(int pos : pp.posP) {
          if(pos == 1 || pos == 2) {
            if(pp.numberElements != "") {
              this->_repeat = pp.numberElements;
              break;
            }
          }
        }
      }
      break;
    case POLCA_ZIPWITH:
      for(ParPos &pp : _parPosC) {
        for(int pos : pp.posP) {
          if(pos == 1 || pos == 2 || pos == 3) {
            if(pp.numberElements != "") {
              this->_repeat = pp.numberElements;
              break;
            }
          }
        }
      }
      break;
    case POLCA_FOLDL:
      for(ParPos &pp : _parPosC) {
        for(int pos : pp.posP) {
          if(pos == 2) {
            if(pp.numberElements != "") {
              this->_repeat = pp.numberElements;
              break;
            }
          }
        }
      }
      break;
    default:
      break;
  }
}
void PolcaScope::matchMemInfoParPragma(std::vector<ParChild> *pvars) {
  for(ParPos &pp : _parPosC) {
    for(ParChild &pc : *pvars) {
      if(pc.parPragmaName == pp.var) {
        pp.numberElements = pc.numberElements;
        pp.sizeElement = pc.sizeElement;
      }
    }
  }
}
