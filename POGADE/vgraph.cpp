#include "vgraph.h"
#include "gsnode.h"

#include <string>
#include <iostream>

#include <QDebug>
#include <QStringList>

VGraph::VGraph(QObject *parent) : QObject(parent)
{
  _fontMetrics = new QFontMetrics(_font);
}

VGraph::~VGraph() {
  delete _fontMetrics;
}

void VGraph::generateGraph(PogadeProjectSourceFile *sf, int scopeId) {
  _sf = sf;
  generateGraph(scopeId);
}

void VGraph::generateGraph(int scopeId) {
  if(_G) {
    delete _G;
    _G = nullptr;
  }

  if(_GA) {
    delete _GA;
    _GA = nullptr;
  }

  _G = new Graph();

  _GA = new GraphAttributes();
  /*
  _GA->init(*_G, GraphAttributes::nodeGraphics |
            GraphAttributes::edgeGraphics |
            GraphAttributes::nodeLabel |
            GraphAttributes::nodeStyle |
            GraphAttributes::edgeType  |
            GraphAttributes::edgeArrow |
            GraphAttributes::edgeStyle);
  */

  _GA->init(*_G,
            GraphAttributes::nodeGraphics |
            GraphAttributes::edgeGraphics |
            GraphAttributes::nodeLabel);


  if(scopeId < 0) {
    std::vector<int> rNodesIndex = _sf->rootScopes();
    for(int id : rNodesIndex) {
      PolcaScope *ps = _sf->findScope(id);
      node n = _G->newNode(id);

      QString s = ps->name();
      if(ps->nPragmas())
        s += '\n' + ps->pragmaTextAll();

      _GA->label(n) = s.toLatin1().toStdString();
      int _n = s.split(QRegExp("\n|\r\n|\r")).size();
      _GA->height(n) = _fontMetrics->height() * _n;
      _GA->width(n) = sizeLongestString(s) + 10;

      // TODO: Process Internal nodes
    }
  }
  else {
    PolcaScope *mains = _sf->findScope(scopeId);
    std::vector<int> children = mains->children();

    for(int child : children) {
      int id = child;
      PolcaScope *ps = _sf->findScope(id);
      node n = _G->newNode(id);

      QString s = ps->name();
      if(ps->nPragmas())
        s += '\n' + ps->pragmaTextAll();

      _GA->label(n) = s.toLatin1().toStdString();
      int _n = s.split(QRegExp("\n|\r\n|\r")).size();
      _GA->height(n) = _fontMetrics->height() * _n;
      _GA->width(n) = sizeLongestString(s) + 10;

      // TODO: Process Internal nodes
    }

    /*
    for(ScopeNeighbourInfo neighbour : neighbours) {
      int id = neighbour.id();
      PolcaScope *ps = _sf->findScope(id);
      node n = _G->newNode(id);

      QString s = ps->name();
      if(ps->nPragmas())
        s += '\n' + ps->pragmaTextAll();

      _GA->label(n) = s.toLatin1().toStdString();
      int _n = s.split(QRegExp("\n|\r\n|\r")).size();
      _GA->height(n) = _fontMetrics->height() * _n;
      _GA->width(n) = sizeLongestString(s) + 10;

      // TODO: Process Internal nodes
    }


    for(ScopeNeighbourInfo neighbour : neighbours) {
      int id = neighbour.id();
      PolcaScope *ps = _sf->findScope(id);
      node n = findNode(id);

      //Edges:
      std::vector<int> coutids = neighbour.outChildren();
      for(int coutid : coutids) {
        node w = findNode(coutid);
        if(w) {
          // TODO: do something with the edge?
          edge e = _G->newEdge(n, w);
          qDebug() << "EDGE: " << n->index() << " - " << w->index();
        }
      }

      // TODO: Process Internal nodes
    }
    */
  }
}

node VGraph::findNode(int id) {
  node v;
  forall_nodes(v, *_G){
    if(v->index() == id) {
      return v;
    }
  }

  return nullptr;
}

void VGraph::processGraph() {
  node v;
  forall_nodes(v, *_G){ // iterate through all the node in the graph
    //_GA->fillColor(v) = Color("#FFFF00"); // set node color to yellow
    //_GA->shape(v) = ogdf::Shape::shRect;
  }

  //FIXME: probably these is not needed
  edge e;
  forall_edges(e, *_G) // set default edge color and type
  {
    _GA->bends(e);
    //_GA->arrowType(e) = ogdf::EdgeArrow::eaNone;
    //_GA->strokeColor(e) = Color("#0000FF");
  }

//  SugiyamaLayout SL; //Compute a hierarchical drawing of G (using SugiyamaLayout)
//  SL.setRanking(new OptimalRanking);
//  SL.setCrossMin(new MedianHeuristic);

//  OptimalHierarchyLayout *ohl = new OptimalHierarchyLayout;

//  SL.setLayout(ohl);
//  SL.call(_GA);

  //PlanarizationGridLayout PGL; //Compute a layout
  PlanarizationLayout PGL;
  PGL.call(*_GA);
}

/*
void VGraph::pragmaSelect(int id) {
  emit pragmaSelected(id);
}

void VGraph::pragmaSelectNode(int id) {
  emit pragmaSelectedNode(id);
}
*/

void VGraph::printGraph(GraphScene *gs) {
  int width  = _GA->boundingBox().width();
  int height = _GA->boundingBox().height();

  // NOTE: in sample it uses node width + height and not 50
  //_GA.setAllWidth(50);
  //_GA.setAllHeight(50);

  //gs->setSceneRect(QRect(0, 0, width+50, height+50));
  gs->setSceneRect(QRect(0, 0, width, height));

// TODO: process edges!
  ogdf::edge e;
  forall_edges(e, *_G){
      ogdf::node source = e->source(), target = e->target();
      int x1 = _GA->x(source) + _GA->width(source)/2;
      int y1 = _GA->y(source) + _GA->height(source)/2;
      int x2 = _GA->x(target) + _GA->width(target)/2;
      int y2 = _GA->y(target) + _GA->height(target)/2;

      DPolyline& points = _GA->bends(e);

      List<DPoint>::const_iterator iter = points.begin();
      if(iter != points.end()) {
        QPointF startPoint((*iter).m_x, (*iter).m_y);
        QPainterPath path(startPoint);

        for(;iter != points.end(); ++iter) {
          DPoint dp = *iter;
          QPointF nextPoint(dp.m_x, dp.m_y);
          path.lineTo(nextPoint);
        }

        //gs->addPath(path, QPen(QColor("green")));
        gs->addPath(path);


        List<DPoint>::iterator arrowStartPoint =
                points.get(points.size() - 2);
        List<DPoint>::iterator arrowEndPoint =
            points.get(points.size() - 1);


        drawArrow(gs, QPointF((*arrowStartPoint).m_x, (*arrowStartPoint).m_y),
                  QPointF((*arrowEndPoint).m_x, (*arrowEndPoint).m_y));

      }

  }


  // Printing the nodes
  ogdf::node n;
  forall_nodes(n, *_G) {
      double x = _GA->x(n);
      double y = _GA->y(n);
      double w = _GA->width(n);
      double h = _GA->height(n);

      // TODO set correctly the shape in _GA
      GSNode *gn = new GSNode();
      gn->setNodeShape(NodeShape::RECTANGLE);
      //gn->setNodeShape(NodeShape::HEXAGON);
      //gn->setNodeShape(NodeShape::ELLIPSE);


      connect(gn, SIGNAL(scopeSelectedDown(int)),
              this, SLOT(scopeSelectedDownProcess(int)));
      connect(this, SIGNAL(scopeSelectedUp(int)),
              gn, SLOT(selectedNode(int)));

      connect(gn, SIGNAL(scopeExpandedDown(int)),
              this, SLOT(scopeExpandedDownProcess(int)));

      connect(gn, SIGNAL(rightClick(int, QPoint)),
              this, SIGNAL(rightClickNode(int, QPoint)));

      gn->setPosSize(x, y, w, h);
      gn->setText(QString::fromStdString(_GA->label(n)));
      gn->setId(n->index());


//      QRectF boundingRect(x, y, w, h);
//      cout << x << " : " << y << " : " << endl;

//      QRadialGradient radialGradient(boundingRect.center(), boundingRect.width());
//      radialGradient.setColorAt(1.0, Qt::lightGray);
//      radialGradient.setColorAt(0.7, QColor(230,230,240));
//      radialGradient.setColorAt(0.0, Qt::white);

//      (void) ui->graphView->scene()->addEllipse(boundingRect, QPen(Qt::black), QBrush(QRadialGradient(radialGradient)));
//      QGraphicsTextItem *text = ui->graphView->scene()->addText(QString(GA.labelNode(n).cstr()));
//      text->setPos(x, y);

      gs->addItem(gn);
  }
}

bool VGraph::exportGraph(string file, ExportType type) {
  if(!_G)
    return false;

  bool allGood;
  switch(type) {
    case ExportType::GML:
      allGood = GraphIO::writeGML(*_GA, file);
      break;
    case ExportType::DOT:
      allGood = GraphIO::writeDOT(*_GA, file);
      break;
    case ExportType::SVG:
      allGood = GraphIO::drawSVG(*_GA, file);
      break;
    default:
      allGood = false;
      break;
  }

  return allGood;
}

void VGraph::drawArrow(GraphScene *gs, const QPointF& start, const QPointF& end)
{
    qreal Pi = 3.14;
    qreal arrowSize = 10;

    QPolygonF arrowHead;

    QLineF line(end, start);

    double angle = ::acos(line.dx() / line.length());

    if ( line.dy() >= 0 )
        angle = (Pi * 2) - angle;

    QPointF arrowP1 = line.p1() + QPointF(sin(angle+Pi/3)*arrowSize,
            cos(angle+Pi/3)*arrowSize);
    QPointF arrowP2 = line.p1() + QPointF(sin(angle+Pi-Pi/3)*arrowSize,
            cos(angle+Pi-Pi/3)*arrowSize);


    arrowHead.clear();
    arrowHead << line.p1() << arrowP1 << arrowP2;

    gs->addPolygon(arrowHead);
    gs->addLine(line);
    //scene_->addPolygon(arrowHead, QPen(color), QBrush(color));
    //scene_->addLine(line, QPen(color));
}

void VGraph::scopeSelectedDownProcess(int id) {
  emit scopeSelectedDown(id);
}

void VGraph::scopeSelectedUpProcess(int id) {
  emit scopeSelectedUp(id);
}

void VGraph::scopeExpandedDownProcess(int id) {
  emit scopeExpandedDown(id);
}

int VGraph::sizeLongestString(QString s) {
  int maxSize = 0;
  QStringList sl = s.split(QRegExp("\n|\r\n|\r"));


  for(QString _s : sl) {
    int _size = _fontMetrics->width(_s);
    if(_size > maxSize)
      maxSize = _size;
  }

  return maxSize;
}
