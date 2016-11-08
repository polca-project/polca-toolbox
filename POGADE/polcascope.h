#ifndef POLCASCOPE_H
#define POLCASCOPE_H

#include <vector>
#include <QString>
#include <QTreeWidgetItem>

#include "polcapragma.h"
#include "scopeneighbourinfo.h"


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

  int id();
  QString name();
  int codeLineStart();
  int codeLineEnd();

  int parent();
  void setParent(int parent);
  void clearParent();

  void addChildScope(int child);
  void clearChildren();

  void setRoot(bool root);
  bool root();
  bool isInChildren(int id);
  std::vector<int> children();

  void addNeighbourScope(ScopeNeighbourInfo neighbour);
  void clearNeighbours();
  std::vector<ScopeNeighbourInfo> neighbours();

  QString pragmaTextAll();

/***************************************/
  static int idNext();
  static void idNextReset();
  static void idNextIncrease();
/***************************************/

  static bool ainb(PolcaScope& a, PolcaScope& b);

private:
  std::vector<PolcaPragma> _pragmas;
  std::vector<int> _children;
  int _parent;
  std::vector<ScopeNeighbourInfo> _neighbours;
  int _id;
  QString _name;
  int _codeLineStart;
  int _codeLineEnd;
  bool _root;

  static int _idCount;
};

#endif // POLCASCOPE_H
