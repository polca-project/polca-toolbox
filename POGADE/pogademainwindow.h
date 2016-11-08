#ifndef POGADEMAINWINDOW_H
#define POGADEMAINWINDOW_H

#include <QMainWindow>
#include <QDockWidget>
#include <QTranslator>

#include "pogadeproject.h"
#include "pogadesourcecodeeditor.h"
#include "pogadescopelist.h"

namespace Ui {
class PogadeMainWindow;
}

class PogadeMainWindow : public QMainWindow
{
  Q_OBJECT

public:
  explicit PogadeMainWindow(QWidget *parent = 0);
  ~PogadeMainWindow();
  /* User Defined*/
  void start();
  void removeSourceDock(QDockWidget *dock);
  //void addGraphViewer(PogadeGraphViewer *gv, PogadeProjectSourceFile *source);

public slots:
  void newGV(PogadeProjectSourceFile*);
  void newST(PogadeProjectSourceFile*);

protected:
  void closeEvent(QCloseEvent*);

private slots:
  void exitProgram();
  void openProject();
  void newProject();
  void closeProject();
  void setProject(PogadeProject *loadedProject);
  void saveProject();
  void saveProjectAll();
  void exportProject();
  void importProject();
  void openSourceFile(PogadeProjectSourceFile *source);
  void test();
  void showAbout();
  void showHelp();
  void showSettings();
  void changeLanguage(int langCode);
  void reEnableMe();
  void loadRecentProject();

private:
  Ui::PogadeMainWindow *ui;
  /* User Defined*/
  bool _allCorrect = true;
  bool _validProject = false;
  PogadeProject *_project = nullptr;
  void setUpProject();
  QDockWidget *_dockProjectViewer = nullptr;
  QList<QDockWidget*> _dockSourceEditorList;
  QList<QDockWidget*> _dockGraphViewList;
  QList<QDockWidget*> _dockScopeListList;
  QMenu* menuCodeEditor;
  QMenu* menuGraphView;
  QMenu* menuScopeTree;
  QTranslator *_translator = nullptr;
  bool _polcaToolsAvailable = false;
  int checkPolcaTool();
};

#endif // POGADEMAINWINDOW_H
