#include "pogadeprojectviewer.h"
#include "ui_pogadeprojectviewer.h"

#include <QDebug>
#include <QFileDialog>

PogadeProjectViewer::PogadeProjectViewer(QWidget *parent, PogadeProject *project) :
  QWidget(parent),
  ui(new Ui::PogadeProjectViewer) {
  ui->setupUi(this);
  _project = project;
  setGUI();

  ui->fileTree->setContextMenuPolicy(Qt::CustomContextMenu);

  connect(ui->fileTree, SIGNAL(itemSelectionChanged()),
          this, SLOT(fileTreeSelectionChanged()));
  connect(ui->fileTree, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),
          this, SLOT(treeDoubleClick(QTreeWidgetItem*,int)));

  connect(ui->fileTree, SIGNAL(customContextMenuRequested(QPoint)),
          this, SLOT(showFileMenu2(QPoint)));
}

PogadeProjectViewer::~PogadeProjectViewer() {
  delete ui;
}

void PogadeProjectViewer::setProject(PogadeProject *project) {
  _project = project;
  setGUI();
}

void PogadeProjectViewer::showFileMenu2(const QPoint&  point) {
  qDebug() << "TEST";
  qDebug() << point;
}

void PogadeProjectViewer::setGUI() {
  if(_project) {
    ui->labelName->setText(_project->name());
    ui->fileTree->clear();
    auto codeFiles = _project->codeFiles();
    for(int i=0; i<codeFiles.size(); ++i) {
      PogadeProjectSourceFile *file = codeFiles.at(i);
      addTreeElement(file);
    }
    ui->buttonAddFile->setEnabled(true);
  }
  else {
    ui->labelName->setText("");
    ui->buttonAddFile->setEnabled(false);
  }
  ui->buttonRemove->setEnabled(false);
}

void PogadeProjectViewer::addFile() {
  QStringList fileNames = QFileDialog::getOpenFileNames(this, tr("Select one or more files"),
                                       QDir::homePath(), tr("Code files (*.c *cpp *h *hpp);; Any file (*.*)"));

  if(!(fileNames.empty())) {
    for(int i=0; i < fileNames.size(); ++i) {
      _project->addSourceFile(fileNames.at(i));
      setGUI();
    }
  } else {
    qDebug() << "No files selected";
  }
}

void PogadeProjectViewer::addTreeElement(PogadeProjectSourceFile *element) {
  QTreeWidgetItem *treeItem = new QTreeWidgetItem(ui->fileTree);
  treeItem->setData(0,1, QVariant(element->id()));
  treeItem->setText(0, element->name());
}

void PogadeProjectViewer::removeItem(){
  QTreeWidgetItem *item = ui->fileTree->currentItem();
  int i = ui->fileTree->indexOfTopLevelItem(item);
  ui->fileTree->takeTopLevelItem(i);
  QVariant v = item->data(0, 1);

  qDebug() << "Remove file: " << item->text(0) << " - ID: " << v.toInt();

  _project->removeFile(v.toInt());

  delete item;
}

void PogadeProjectViewer::fileTreeSelectionChanged() {
  ui->buttonRemove->setEnabled(true);
}

void PogadeProjectViewer::treeDoubleClick(QTreeWidgetItem * item, int column) {
  QVariant v = item->data(0, 1);
  PogadeProjectSourceFile * source = _project->getSourceFile((unsigned int) v.toInt());
  emit openSourceFile(source);
  //qDebug() << "Double CLICK";
}
