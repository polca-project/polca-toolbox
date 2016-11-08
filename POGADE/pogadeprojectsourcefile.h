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
  QString code();
  QString codeRev(unsigned int rev);
  unsigned int nextRevision();
  QDockWidget *getDock();
  QDockWidget *getDockVisual();
  QDockWidget *getDockScopeTree();
  int getRevInUse();
  int setRevInUse(int rev);
  int saveCurrentRevision(QString text);
  QString getRevisionName(unsigned int rev);
  void addScope(PolcaScope ps);
  std::vector<PolcaScope> getScopes();
  void clearScopes();
  void setValidScope(bool valid);
  bool validScope();
  //void analyzeCurrentScopes();
  void findRootScopes();
  void generateTreeScopes();
  void automaticNamesScopes();
  PolcaScope* findScope(int id);
  PolcaScope* findScope(int startLine, int endLine);
  std::vector<int> rootScopes();
  PogadeProject* getProject();

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
  PogadeProject* _project;
  QDockWidget* _dock = nullptr;
  QDockWidget* _dockVisual = nullptr;
  QDockWidget* _dockScopeTree = nullptr;
  std::vector<PolcaScope> _scopes;
  bool _validScopes = false;
};

#endif // POGADEPROJECTSOURCEFILE_H
