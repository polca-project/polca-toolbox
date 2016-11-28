#ifndef POGADEPROJECTSOURCEFILE_H
#define POGADEPROJECTSOURCEFILE_H

#include <QString>
#include <QList>
#include <QJsonObject>
#include <QJsonValue>
#include <QJsonArray>
#include <QDockWidget>
#include <vector>
#include <tuple>

#include "polcascope.h"
#include "polcatransformation.h"
#include "asmcountdata.h"


class PogadeProject;

class PogadeProjectSourceFile
{
public:
  PogadeProjectSourceFile(PogadeProject* project);
  PogadeProjectSourceFile(PogadeProject* project, QJsonObject data);
  PogadeProjectSourceFile(PogadeProject* project, QString name, unsigned int id);
  ~PogadeProjectSourceFile();
  std::tuple<bool, PolcaScope, PolcaPragma> findScopeAndPragmaFromLine(int line);

  QString name();
  QList<unsigned int> revisions();
  QList<QString> revisionsNames();
  int addRevision(int orgRev, QString revName);
  void removeRevision(unsigned int rev);
  QJsonValue toJson();
  unsigned int id();
  bool opened();
  void setOpen(bool open, QDockWidget* dock);
  bool visualized();
  void setVisualized(bool visualized, QDockWidget* dock);
  bool scopeTree();
  void setScopeTree(bool scopeTree, QDockWidget* dock);
  void setTransformationView(bool transformationView, QDockWidget* dock);

  bool scopeTransformations();
  void setScopeTransformations(bool scopeTransformations, QDockWidget* dock);
  QString code();
  QString codeRev(unsigned int rev);
  unsigned int nextRevision();
  QDockWidget *getDock();
  QDockWidget *getDockVisual();
  QDockWidget *getDockScopeTree();
  int getRevInUse();
  int setRevInUse(unsigned int rev);
  int saveCurrentRevision(QString text);
  QString getRevisionName(unsigned int rev);
  void addScope(PolcaScope ps);
  std::vector<PolcaScope> getScopes();
  void clearScopes();
  void setValidScope(bool valid);
  bool validScope();
  void findRootScopes();
  std::vector<ScopeChild> generateOrderedRootChildren();
  void generateTreeScopes();
  void automaticNamesScopes();
  void processIOparamsScopes();
  void propagateMemoryInfo();

  void clearRootMemory();
  std::vector<MemInfo> rootMemory();
  void addRootMemory(MemInfo mem);
  MemInfo* findMemory(QString name);

  PolcaScope findScopeAndCopy(QString name);
  PolcaScope* findScope(QString name, int line);
  PolcaScope* findScope(int id);
  PolcaScope* findScope(int startLine, int endLine);
  PolcaScope* findScopeFromLine(int line);
  std::vector<PolcaScope*> findScopes(QString name);
  void linkScopeChildren();

  std::vector<int> rootScopes();
  PogadeProject* getProject();
  void setMemoryShow(bool show);
  bool memoryShow();
  void setRootMemory();

  void clearTransformations();
  void addTransformation(PolcaTransformation pt);
  std::vector<PolcaTransformation> getTransformations();
  PolcaTransformation* transformation(int id);

  std::vector<PolcaTransformation*> transformationsLine(int line);

  void clearASMCData();
  std::vector<ASMCData> getASMCData();
  void setASMCData(std::vector<ASMCData> data);

  void setMemoryInScopes();

private:
  unsigned int _latestRev = 0;
  unsigned int _id;
  int _revInUse = -1;
  QString _name = "";
  QList<unsigned int> _revisions;
  QList<unsigned int> _revisionsFather;
  QList<QString> _revisionsNames;
  bool _opened = false;
  bool _visualized = false;
  bool _scopeTree = false;
  bool _transformationView = false;
  bool _scopeTransformations = false;
  PogadeProject* _project;
  QDockWidget* _dock = nullptr;
  QDockWidget* _dockVisual = nullptr;
  QDockWidget* _dockScopeTree = nullptr;
  QDockWidget* _dockTransformationView = nullptr;
  QDockWidget* _dockTransformations = nullptr;
  std::vector<PolcaScope> _scopes;
  bool _validScopes = false;
  bool _showMemory = true;
  //std::vector<ScopeChild> _rootChildren;
  std::vector<MemInfo> _rootMemory;
  ///////////////
  std::vector<PolcaTransformation> _transformations;
  std::vector<ASMCData> _asmcData;
};

#endif // POGADEPROJECTSOURCEFILE_H
