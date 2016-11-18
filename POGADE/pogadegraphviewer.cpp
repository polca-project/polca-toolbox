#include "pogadegraphviewer.h"
#include "ui_pogadegraphviewer.h"

#include <QDebug>
#include <QMenu>

#include "graphexportdialog.h"

PogadeGraphViewer::PogadeGraphViewer(QWidget *parent) :
  QWidget(parent),
  ui(new Ui::PogadeGraphViewer)
{
  ui->setupUi(this);
  ui->buttonExport->setEnabled(false);
  _gs = new GraphScene();
  ui->viewer->setScene(_gs);

  connect(&_graph, SIGNAL(scopeSelectedDown(int)),
          this, SLOT(scopeSelectedDownProcess(int)));
  connect(this, SIGNAL(scopeSelectedUp(int)),
          &_graph, SLOT(scopeSelectedUpProcess(int)));

  connect(&_graph, SIGNAL(scopeExpandedDown(int)),
          this, SLOT(scopeExpandedDownProcess(int)));

  connect(&_graph, SIGNAL(rightClickNode(int,QPoint)),
          this, SLOT(rightClickNode(int,QPoint)));

  connect(ui->viewer, SIGNAL(customContextMenuRequested(QPoint)),
          this, SLOT(showContextMenu(QPoint)));
}

PogadeGraphViewer::~PogadeGraphViewer() {
  delete ui;
  delete _gs;
}

void PogadeGraphViewer::setSourceFile(PogadeProjectSourceFile *sf) {
  _sf = sf;
  updateGUI();
}

PogadeProjectSourceFile *PogadeGraphViewer::sourceFile() {
  return _sf;
}

void PogadeGraphViewer::exportGraph() {
  // TODO: finish this
  GraphExportDialog *w = new GraphExportDialog(&_graph);
  w->show();
}

void PogadeGraphViewer::updateGUI() {
  if(_sf) {
    generateScene(-1);
    ui->buttonExport->setEnabled(true);
  }
  else {
    ui->buttonExport->setEnabled(false);
  }
}

void PogadeGraphViewer::generateScene(int id) {
  // TODO: finish this
  _graph.generateGraph(_sf, id);
  _graph.processGraph();
  _gs->clear();
  _graph.printGraph(_gs);
}

void PogadeGraphViewer::scopeSelectedDownProcess(int id) {
  emit scopeSelectedDown(id);
}

void PogadeGraphViewer::scopeSelectedUpProcess(int id) {
  emit scopeSelectedUp(id);
  _graph.printGraph(_gs);
}

void PogadeGraphViewer::scopeExpandedDownProcess(int id) {
  this->generateScene(id);
}

void PogadeGraphViewer::showContextMenu(const QPoint& pos) {
  qDebug() << pos;
}

void PogadeGraphViewer::rightClickNode(int id, QPoint p) {
  qDebug() << id << " - " << p;
  _workingNode = id;

  QMenu menu(this);
  menu.addAction(tr("Expand Node"), this, SLOT(expandNode()));
  menu.addAction(tr("Expand Node in new Window"), this, SLOT(expandNodeNewWindow()));
  menu.exec(p);

  connect(&menu, SIGNAL(destroyed(QObject*)), this, SLOT(test()));
}


void PogadeGraphViewer::expandNode() {
  scopeSelectedUpProcess(_workingNode);
  scopeExpandedDownProcess(_workingNode);
}

void PogadeGraphViewer::expandNodeNewWindow() {

}

void PogadeGraphViewer::test() {

}
