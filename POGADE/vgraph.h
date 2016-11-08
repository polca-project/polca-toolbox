#ifndef VGRAPH_H
#define VGRAPH_H

#include <QObject>
#include <QFont>
#include <QApplication>
#include <QFontMetrics>
#include <QString>

#include <ogdf/basic/Graph.h>
#include <ogdf/basic/graph_generators.h>
#include <ogdf/layered/DfsAcyclicSubgraph.h>
#include <ogdf/fileformats/GraphIO.h>
#include <ogdf/layered/SugiyamaLayout.h>
#include <ogdf/planarity/PlanarizationLayout.h>
#include <ogdf/layered/OptimalRanking.h>
#include <ogdf/layered/MedianHeuristic.h>
#include <ogdf/layered/OptimalHierarchyLayout.h>
#include <ogdf/planarity/PlanarizationGridLayout.h>

#include <vector>

#include "pogadeprojectsourcefile.h"
#include "graphscene.h"

using namespace ogdf;

enum class ExportType{GML, DOT, SVG};

class VGraph : public QObject
{
  Q_OBJECT
public:
  explicit VGraph(QObject *parent = 0);
  ~VGraph();

  void generateGraph(PogadeProjectSourceFile *sf, int scopeId);
  void generateGraph(int scopeId);
  void processGraph();
  void printGraph(GraphScene *gs);

signals:
  void scopeSelectedDown(int);
  void scopeSelectedUp(int);
  void scopeExpandedDown(int);
  void rightClickNode(int, QPoint);

public slots:
  bool exportGraph(string file, ExportType type);
  void scopeSelectedDownProcess(int id);
  void scopeSelectedUpProcess(int id);
  void scopeExpandedDownProcess(int id);

private:
  PogadeProjectSourceFile *_sf = nullptr;
  QFont _font = QApplication::font();
  QFontMetrics *_fontMetrics;

  Graph *_G = nullptr;
  GraphAttributes *_GA = nullptr;

  node findNode(int id);
  int sizeLongestString(QString s);
  void drawArrow(GraphScene *gs, const QPointF& start, const QPointF& end);
};

#endif // VGRAPH_H
