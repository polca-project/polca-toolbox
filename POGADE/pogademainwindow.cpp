#include "pogademainwindow.h"
#include "ui_pogademainwindow.h"

#include "pogadedefines.h"
#include "newprojectdialog.h"
#include "pogadeprojectviewer.h"
#include "pogadeimport.h"
#include "pogadesettings.h"
#include "pogadescopelist.h"
#include "pogadesourcecodeeditor.h"
#include "pogadetransformationview.h"
#include "polcascope.h"

#include <iostream>
#include <QDebug>
#include <QFileDialog>
#include <QMessageBox>
#include <QSettings>
#include <QProcess>


PogadeMainWindow::PogadeMainWindow(QWidget *parent) :
                  QMainWindow(parent),
                  ui(new Ui::PogadeMainWindow) {
  QSettings settings;
  restoreGeometry(settings.value("mainWindowGeometry").toByteArray());

  changeLanguage(-1);

  ui->setupUi(this);

  _polcaToolsAvailable = (this->checkPolcaTool() >= 0);
  if(!_polcaToolsAvailable)
    QMessageBox::critical(this, MYNAME, tr("No POLCA Tools commands found<br>Please check Settings"));

  setUpProject();
  PolcaScope::idNextReset();

  // Set up Menus
  menuCodeEditor = ui->menuView->addMenu(tr("Code Editors"));
  menuCodeEditor->setEnabled(false);

  menuGraphView = ui->menuView->addMenu(tr("Graph Viewers"));
  menuGraphView->setEnabled(false);

  menuScopeTree = ui->menuView->addMenu(tr("Scope Tree"));
  menuScopeTree->setEnabled(false);

  menuTransormations = ui->menuView->addMenu(tr("Transformations Viewer"));
  menuTransormations->setEnabled(false);

  restoreState(settings.value("mainWindowState").toByteArray());
  settings.setValue("mainWindowGeometry", saveGeometry());

  QString latestProjectName = settings.value("recentProjectName").toByteArray();
  //QString latestProjectPath = settings.value("recentProjectFile").toByteArray();

  if(latestProjectName == "")
    ui->actionRecent->setEnabled(false);
  else {
    ui->actionRecent->setText("Load " + latestProjectName);
  }
}

PogadeMainWindow::~PogadeMainWindow() {
  delete ui;
}

/************************/
/* User Defined Methods */
/************************/

void PogadeMainWindow::start() {
  if(_allCorrect) {
    this->show();
  }
  else {
    this->close();
    exit(1);
  }
}

void PogadeMainWindow::exitProgram() {
//TODO: Ask to save project befor exit
  this->close();
}

void PogadeMainWindow::openProject() {
  QString fileName = QFileDialog::getOpenFileName(this, tr("Open a Pogade Project"),
                                                  QDir::homePath(), tr("Pogade Project Files (*.pof)"));

  if(fileName.size() > 2) {
    qDebug() << "Open Project: " << fileName;

    if(_project) {
      //TODO: offer to save current open project before opening a new one
      delete _project;
    }

    _project = new PogadeProject();
    _project->openProjectFile(fileName);
    setUpProject();
  }
}

void PogadeMainWindow::newProject() {
  NewProjectDialog npd(this);

  npd.setWindowTitle(tr("New Project"));

  connect(&npd, SIGNAL(newProjectCreated(PogadeProject*)),
          this, SLOT(setProject(PogadeProject*)));
  npd.exec();
}

void PogadeMainWindow::closeProject() {
  //TODO: finish it
  QListIterator<QDockWidget*> i(_dockSourceEditorList);
  while (i.hasNext()) {
    PogadeSourceCodeEditor* w = (PogadeSourceCodeEditor*) i.next()->widget();

    bool saveall = false;
    //TODO: check changed and ask user
    if(w->fileChanged()) {
      if(saveall) {
        w->saveFile();
      }
      else {
        QMessageBox msgBox;
        msgBox.setText(tr("The File %1 revision %2 ( %3 ) has been modified and not saved").arg(w->sourceFile()->name()).arg(w->sourceFile()->getRevInUse()).arg(w->sourceFile()->getRevisionName(w->sourceFile()->getRevInUse())));
        msgBox.setInformativeText(tr("Do you want to save your changes before changing revision?"));
        msgBox.setStandardButtons(QMessageBox::Save | QMessageBox::SaveAll | QMessageBox::Discard | QMessageBox::Cancel);
        msgBox.setDefaultButton(QMessageBox::SaveAll);
        int ret = msgBox.exec();
        switch (ret) {
          case QMessageBox::SaveAll:
            saveall = true;
          case QMessageBox::Save:
            w->saveFile();
            break;
          case QMessageBox::Discard:
            break;
          case QMessageBox::Cancel:
            return;
            break;
          default:
            // should never be reached
            break;
        }
      }
    }
  }

  //TODO: clean the project stuff
  _dockProjectViewer->setVisible(false);
  delete _dockProjectViewer->widget();
  delete _dockProjectViewer;
  _dockProjectViewer = nullptr;

  while(!_dockSourceEditorList.isEmpty()) {
      QDockWidget* dock = _dockSourceEditorList.takeFirst();
      dock->setVisible(false);
      PogadeSourceCodeEditor* w = (PogadeSourceCodeEditor*) dock->widget();
      delete w;
    }

  _dockSourceEditorList.clear();
  _allCorrect = true;
  _validProject = false;
  delete _project;
  _project = nullptr;

  this->setUpProject();
}

void PogadeMainWindow::setProject(PogadeProject *loadedProject) {
  _project = loadedProject;
  setUpProject();

  //TODO: Load project
}

void PogadeMainWindow::saveProject() {
  if(_project)
    _project->saveConfFile();
}

void PogadeMainWindow::saveProjectAll() {
  if(_project) {
    QListIterator<QDockWidget*> i(_dockSourceEditorList);
    while (i.hasNext()) {
      PogadeSourceCodeEditor* w = (PogadeSourceCodeEditor*) i.next()->widget();
      if(w->fileChanged())
        w->saveFile();
    }
    _project->saveConfFile();
  }
}

void PogadeMainWindow::setUpProject() {
  if(_project) {
    QSettings settings;
    this->setWindowTitle(_project->name() + " - Pogade");
    _validProject = true;
    if(_dockProjectViewer) {
      delete _dockProjectViewer;
    }
    _dockProjectViewer = new QDockWidget(tr("Project Viewer"));
    _dockProjectViewer->setObjectName("dockProjectViewer");
    QWidget *w = new PogadeProjectViewer(this, _project);
    _dockProjectViewer->setWidget(w);

    _dockProjectViewer->show();
    ui->actionProjectViewer->setEnabled(true);
    //ui->actionCodeEditor->setEnabled(true);

    //TODO: make connections if necesary
    connect(_dockProjectViewer, SIGNAL(visibilityChanged(bool)),
            ui->actionProjectViewer, SLOT(setChecked(bool)));
    connect(ui->actionProjectViewer, SIGNAL(triggered(bool)),
            _dockProjectViewer, SLOT(setVisible(bool)));
    connect(w, SIGNAL(openSourceFile(PogadeProjectSourceFile*)),
            this, SLOT(openSourceFile(PogadeProjectSourceFile*)));

    this->addDockWidget(Qt::TopDockWidgetArea, _dockProjectViewer);

    //qDebug() << "Project Loaded!";

    settings.setValue("recentProjectName", _project->name());
    settings.setValue("recentProjectFile",
                      _project->path() + QDir::separator() + _project->name() + ".pof");
  }
  else {
    this->setWindowTitle("Pogade");
    _validProject = false;
    _dockProjectViewer = nullptr;
  }

  ui->actionSave->setEnabled(_validProject);
  ui->actionSaveAll->setEnabled(_validProject);
  ui->actionSaveAs->setEnabled(_validProject);
  ui->actionCloseProject->setEnabled(_validProject);
  ui->actionExportProject->setEnabled(_validProject);
}

void PogadeMainWindow::openSourceFile(PogadeProjectSourceFile *source) {
  if(!source->opened()) {
    QDockWidget *_newSource = new QDockWidget(tr("Source Editor ") + source->name());
    _newSource->setObjectName("dockSourceEditor" + source->name());
    QWidget *w = new PogadeSourceCodeEditor(this);
    _newSource->setWidget(w);

    PogadeSourceCodeEditor *we = (PogadeSourceCodeEditor *) w;
    we->setSourceFile(source);
    we->setDock(_newSource);
    we->setTDir(_project->getTDir());

    source->setOpen(true, _newSource);
    _dockSourceEditorList << _newSource;
    _newSource->show();
    this->addDockWidget(Qt::TopDockWidgetArea, _newSource);

    // Set up the menus
    menuCodeEditor->setEnabled(true);
    QAction* actionCodeEditor = menuCodeEditor->addAction(source->name());
    actionCodeEditor->setCheckable(true);
    actionCodeEditor->setChecked(true);

    // Make connections
    connect(actionCodeEditor, SIGNAL(toggled(bool)),
            _newSource, SLOT(setVisible(bool)));
    connect(_newSource, SIGNAL(visibilityChanged(bool)),
            actionCodeEditor, SLOT(setChecked(bool)));

    connect(we, SIGNAL(createNV(PogadeProjectSourceFile*)),
            this, SLOT(newGV(PogadeProjectSourceFile*)));
    connect(we, SIGNAL(createST(PogadeProjectSourceFile*)),
            this, SLOT(newST(PogadeProjectSourceFile*)));
    connect(we, SIGNAL(createTR(PogadeProjectSourceFile*)),
            this, SLOT(newTR(PogadeProjectSourceFile*)));
  }
  else {
    //qDebug() << "Lets show Code Editor!";
    QDockWidget *d = source->getDock();
    d->setVisible(true);
  }
}

void PogadeMainWindow::test() {
    //qDebug() << "dock destroyed";
}

void PogadeMainWindow::exportProject() {
  if(_project) {
    QString file = QFileDialog::getSaveFileName(this, tr("Export Pogade Project"),
                               QDir::homePath(), tr("Pogade Project (*.pox)"));

    if(!file.endsWith(".pox", Qt::CaseSensitive)) {
      file.append(".pox");
    }

    if(file.size() > 3) {
      _project->saveConfFile();
      //TODO: save all opened files
      _project->exportProject(file);
    }
  }
}

void PogadeMainWindow::removeSourceDock(QDockWidget *dock) {
  for(int i=0; i<_dockSourceEditorList.size(); ++i) {
    if(dock == _dockSourceEditorList.at(i)) {
      _dockSourceEditorList.removeAt(i);
    }
  }
}

void PogadeMainWindow::importProject() {
  PogadeImport npi(this);
  connect(&npi, SIGNAL(importProject(PogadeProject*)),
          this, SLOT(setProject(PogadeProject*)));
  npi.exec();
}

void PogadeMainWindow::showAbout() {
  QMessageBox::about(this, MYNAME, tr("<h2><b>POLCA Graph Analysis & Development Environment</b></h2><br/>(C) 2015-2016"));
}

void PogadeMainWindow::showHelp() {
  QMessageBox::about(this, MYNAME, tr("<a href='http://www.polca-project.eu'>For help visit the POLCA website</a>"));
}

void PogadeMainWindow::changeLanguage(int langCode) {
  QSettings settings;
  QString file;

  if(langCode < 0)
    langCode = settings.value("language", QLocale().language()).toInt();

  switch(langCode) {
    case QLocale::English:
      file = ":/translations/lang-EN";
      break;
    case QLocale::Spanish:
      file = ":/translations/lang-ES";
      break;
    default:
      file = ":/translations/lang-EN";
      break;
  }

  if(_translator)
    qApp->removeTranslator(_translator);

  _translator = new QTranslator();
  _translator->load(file);

  qApp->installTranslator(_translator);

  settings.setValue("language", langCode);
}

void PogadeMainWindow::closeEvent(QCloseEvent*) {
  QSettings settings;

  settings.setValue("mainWindowGeometry", saveGeometry());
  settings.setValue("mainWindowState", saveState());

  qApp->quit();
}

void PogadeMainWindow::reEnableMe() {
  this->setEnabled(true);
}

void PogadeMainWindow::showSettings() {
  pogadeSettings *w = new pogadeSettings(this);
  connect(w, SIGNAL(destroyed()), this, SLOT(reEnableMe()));
  //this->setEnabled(false);
  w->show();
}

void PogadeMainWindow::loadRecentProject() {
  QSettings settings;
  QString latestProjectPath = settings.value("recentProjectFile").toByteArray();

  qDebug() << "Open Project: " << latestProjectPath;

  if(_project) {
    //TODO: offer to save current open project before opening a new one
    delete _project;
  }

  _project = new PogadeProject();
  _project->openProjectFile(latestProjectPath);
  setUpProject();
}

void PogadeMainWindow::newGV(PogadeProjectSourceFile* source) {
  if(!source->visualized()) {
    QDockWidget *_ng = new QDockWidget(tr("Graph Viewer ") + source->name());
    _ng->setObjectName("dockGraphViewer" + source->name());
    QWidget *w = new PogadeGraphViewer(this);
    _ng->setWidget(w);

    PogadeGraphViewer *wv = (PogadeGraphViewer *) w;
    wv->setSourceFile(source);
    //we->setDockGraphView(_ng);

    source->setVisualized(true, _ng);
    _dockGraphViewList << _ng;
    _ng->show();
    this->addDockWidget(Qt::BottomDockWidgetArea, _ng);

    // Set up the menus
    menuGraphView->setEnabled(true);
    QAction* actionGraphView = menuGraphView->addAction(source->name());
    actionGraphView->setCheckable(true);
    actionGraphView->setChecked(true);

    // Make connections
    connect(actionGraphView, SIGNAL(toggled(bool)),
            _ng, SLOT(setVisible(bool)));

    connect(_ng, SIGNAL(visibilityChanged(bool)),
            actionGraphView, SLOT(setChecked(bool)));

    connect(wv, SIGNAL(scopeSelectedDown(int)),
            (PogadeSourceCodeEditor*)source->getDock()->widget(),
            SLOT(scopeSelectedUPProcess(int)));

    connect((PogadeSourceCodeEditor*)source->getDock()->widget(), SIGNAL(scopeSelectedDown(int)),
            wv, SLOT(scopeSelectedUpProcess(int)));
    connect((PogadeSourceCodeEditor*)source->getDock()->widget(), SIGNAL(repaint()),
            wv, SLOT(updateGUI()));
  }
  else {
    qDebug() << "Lets show Graph View!";
    QDockWidget *d = source->getDockVisual();
    d->setVisible(true);
  }
}

void PogadeMainWindow::newTR(PogadeProjectSourceFile* source) {
  if(!source->scopeTransformations()) {
    QDockWidget *_ntr = new QDockWidget(tr("Transformations ") + source->name());
    _ntr->setObjectName("dockTransformations" + source->name());
    QWidget *w = new PogadeTransformationView(this);
    _ntr->setWidget(w);

    PogadeTransformationView *we = (PogadeTransformationView*) w;
    we->setSourceFile(source);

    source->setScopeTree(true, _ntr);
    _dockTransformationsList << _ntr;
    _ntr->show();
    this->addDockWidget(Qt::BottomDockWidgetArea, _ntr);

    // Set up the menus
    menuTransormations->setEnabled(true);
    QAction* actionTransformations = menuScopeTree->addAction(source->name());
    actionTransformations->setCheckable(true);
    actionTransformations->setChecked(true);

    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
    //////////////////////////////////////////////////
  }
  else {
    qDebug() << "Lets show Transformations View!";
    //QDockWidget *d = source->getDockVisual();
    //d->setVisible(true);
  }
}

void PogadeMainWindow::newST(PogadeProjectSourceFile* source) {
  if(!source->scopeTree()) {
    QDockWidget *_nst = new QDockWidget(tr("Scope Tree ") + source->name());
    _nst->setObjectName("dockScopeTree" + source->name());
    QWidget *w = new PogadeScopeList(this);
    _nst->setWidget(w);

    PogadeScopeList *we = (PogadeScopeList *) w;
    we->setSourceFile(source);
    //we->setDockGraphView(_ng);

    source->setScopeTree(true, _nst);
    _dockScopeListList << _nst;
    _nst->show();
    this->addDockWidget(Qt::BottomDockWidgetArea, _nst);

    // Set up the menus
    menuScopeTree->setEnabled(true);
    QAction* actionScopeTree = menuScopeTree->addAction(source->name());
    actionScopeTree->setCheckable(true);
    actionScopeTree->setChecked(true);

    // Make connections
    connect(actionScopeTree, SIGNAL(toggled(bool)),
            _nst, SLOT(setVisible(bool)));

    connect(_nst, SIGNAL(visibilityChanged(bool)),
            actionScopeTree, SLOT(setChecked(bool)));
    connect((PogadeSourceCodeEditor*)source->getDock()->widget(), SIGNAL(scopeSelectedDown(int)),
            we, SLOT(scopeSelectedUpProcess(int)));
    connect(we, SIGNAL(scopeSelectedDown(int)),
            (PogadeSourceCodeEditor*)source->getDock()->widget(),
            SLOT(scopeSelectedUPProcess(int)));
    connect((PogadeSourceCodeEditor*)source->getDock()->widget(), SIGNAL(repaint()),
            we, SLOT(updateGUI()));

  }
  else {
    qDebug() << "Lets show Graph View!";
    QDockWidget *d = source->getDockVisual();
    d->setVisible(true);
  }
}

int PogadeMainWindow::checkPolcaTool() {
  QProcess tool;
  QSettings settings;


  QString polcaToolCommand = settings.value("PTReader", POLCATOOLREADER).toString();
  tool.start(polcaToolCommand, QStringList());
  if (!tool.waitForStarted(-1))
    return -2;
  if (!tool.waitForFinished(-1))
    return -3;


  polcaToolCommand = settings.value("PTInit", POLCATOOLINIT).toString();
  tool.start(polcaToolCommand, QStringList());
  if (!tool.waitForStarted(-1))
    return -4;
  if (!tool.waitForFinished(-1))
    return -5;


  polcaToolCommand = settings.value("PTApply", POLCATOOLAPPLY).toString();
  tool.start(polcaToolCommand, QStringList());
  if (!tool.waitForStarted(-1))
    return -6;
  if (!tool.waitForFinished(-1))
    return -7;


  return 0;
}
