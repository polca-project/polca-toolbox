#ifndef POLCASCOPE_H
#define POLCASCOPE_H

#include <vector>
#include <algorithm>
#include <QString>
#include <QTreeWidgetItem>
#include <QTemporaryDir>

#include "polcapragma.h"
#include "scopeneighbourinfo.h"

class PolcaScope;

#define PARIN  1
#define PAROUT 2

typedef struct {
  QString name;
  QString eSize;
  QString elements;
} MemInfo;

typedef struct {
  QString fun;
  int scopeId;
  int line;
  std::vector<QString> var;
  std::vector<int> posC;
} FunCall;

typedef struct {
  QString var;
  int posC;
  std::vector<int> posP;
  bool input = false;
  bool output = false;
  QString numberElements = "";
  QString sizeElement = "";
} ParPos;

typedef struct {
  QString parPragmaName;
  QString parName;
  bool input = false;
  bool output = false;
  QString numberElements = "";
  QString sizeElement = "";
} ParChild;

typedef struct {
  int cid;
  int cline;
  PolcaScope *cscope;
  std::vector<ParChild> vars;
} ScopeChild;

#define POLCA_NONE     0x00
#define POLCA_MAP      0x11
#define POLCA_ITN      0x12
#define POLCA_ZIPWITH  0x13
#define POLCA_FOLDL    0x14
#define POLCA_MEMALLOC 0x24
#define POLCA_MEMFREE  0x28

class PolcaScope
{
public:
  PolcaScope();
  ~PolcaScope();

  void pragmasClear();
  void pragmaAdd(PolcaPragma pragma);
  std::vector<PolcaPragma> getPragmas();
  int nPragmas();

  void setId(int id);
  void setName(QString name);
  void setCodeLineStart(int line);
  void setCodeLineEnd(int line);

  bool automaticName();

  void automaticType();
  int getType();

  int id();
  QString name();
  int codeLineStart();
  int codeLineEnd();

  int parentId();
  PolcaScope* parent();
  void setParent(PolcaScope *parent);
  void clearParent();

  void setIsFunction(bool function);
  bool isFunction();

  void setRoot(bool root);
  bool root();
  void addChildScope(int child, int line, PolcaScope* pscope, std::vector<QString> var);
  void clearChildren();
  bool isInChildren(int id);
  std::vector<ScopeChild> children();
  static void linkChildren(std::vector<ScopeChild> sChildren);

  void clearfParameters();
  void addfParameter(ParPos p);
  std::vector<ParPos>* fParameters();
  void procesIOparams();

  void setCallLine(int line);
  int callLine();

  ScopeNeighbourInfo neighbours();

  QString pragmaTextAll();

  void clearChildMemory();
  void addChildMemory(MemInfo mem);
  std::vector<MemInfo> getChildMem();
  MemInfo *findMemName(QString name);

  /***************************************/
  /* STATIC MEMBERS                      */
  /***************************************/
  static int idNext();
  static void idNextReset();
  static void idNextIncrease();
  /***************************************/
  static bool ainb(PolcaScope& a, PolcaScope& b);
  static int stringToType(QString s);
  static QString typeToString(int type);
  /***************************************/
  static bool isParInput(ScopeChild& sc, QString par);
  static bool isParOutput(ScopeChild& sc, QString par);
  /***************************************/
  static ParPos* getParPosWithName(std::vector<ParPos> *list, QString varName);
  /***************************************/
  static std::vector<QStringList> splitParameters(QString params);
  /***************************************/
  std::vector<ParPos> processPragmaIO(int type, std::vector<QStringList> l);

  void printIO(std::vector<ParChild> varsC);
  void getParPragmaFunc(int pos, QString *pragmaName, bool *in, bool *out);

  void setASMWeightMine(int w);
  int  getASMWeightMine();
  QString getASMWeightTotal();
  QString getASMWeightTotalSimp();

  MemInfo getMemoryInfo();

  void matchMemInfoParPragma(std::vector<ParChild> *pvars);
  void setMemoryInfoFromParent();
  void analyzeRepetitions();

  static QTemporaryDir *pogadeTDir;

private:
  std::vector<PolcaPragma> _pragmas;
  std::vector<ScopeChild> _children;
  PolcaScope* _parent = nullptr;
  ScopeNeighbourInfo _neighbours;
  int _id;
  QString _name;
  int _codeLineStart;
  int _codeLineEnd;
  bool _root;

  bool _isFunction = false;
  int _callLine = -1;

  static void setIOPar(ParPos*p, int type);
  std::vector<ParPos> _parPosC;

  int _type = POLCA_NONE;
  static int _idCount;

  int _ASMWeightMine;
  QString _ASMWeightTotal = "";

  QString _repeat = "";

  void addIOInfo(QString var, int type);
  void setIOFromPragma(PolcaPragma pragma);
  MemInfo _mem;
  std::vector<MemInfo> _childMem;
};

#endif // POLCASCOPE_H
