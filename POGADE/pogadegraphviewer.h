#ifndef POGADEGRAPHVIEWER_H
#define POGADEGRAPHVIEWER_H

#include <QWidget>
#include "pogadeprojectsourcefile.h"
#include "polcascope.h"
#include "vgraph.h"
#include "graphscene.h"

namespace Ui {
  class PogadeGraphViewer;
}

class PogadeGraphViewer : public QWidget
{
  Q_OBJECT

public:
  explicit PogadeGraphViewer(QWidget *parent = 0);
  ~PogadeGraphViewer();
  void setSourceFile(PogadeProjectSourceFile *sf);
  PogadeProjectSourceFile *sourceFile();
  //void setDockGraphView(QDockWidget *dock);
  //QDockWidget * getDockGraphView();

public slots:
  void updateGUI();

private slots:
  void exportGraph();
  void generateScene(int id);
  void scopeSelectedDownProcess(int id);
  void scopeSelectedUpProcess(int id);
  void scopeExpandedDownProcess(int id);
  void showContextMenu(const QPoint& pos);
  void rightClickNode(int, QPoint);
  void expandNode();
  void expandNodeNewWindow();
  void drawRoot();
  void test();

signals:
  void scopeSelectedDown(int);
  void scopeSelectedUp(int);

protected:
  //void mousePressEvent(QGraphicsSceneMouseEvent *event);

private:
  Ui::PogadeGraphViewer *ui;
  PogadeProjectSourceFile *_sf = nullptr;
  QDockWidget *_dock = nullptr;
  VGraph _graph;
  GraphScene *_gs = nullptr;
  int _workingNode = -1;
  int _lastNode = -1;
};

#endif // POGADEGRAPHVIEWER_H
