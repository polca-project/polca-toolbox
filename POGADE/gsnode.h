#ifndef GSNODE_H
#define GSNODE_H

#include <QGraphicsItem>
#include <QGraphicsWidget>
#include <QPainter>
#include <QGraphicsSceneMouseEvent>
#include <QObject>
#include <QWidget>


enum class NodeShape{RECTANGLE, TRIANGLE, HEXAGON, ELLIPSE};

class GSNode : public QObject, public QGraphicsItem
{
  Q_OBJECT
  Q_INTERFACES(QGraphicsItem)

public:
  //GSNode(GraphWidget *graphWidget);

  QRectF boundingRect() const;
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);

  void setPosSize(double x, double y, double w, double h);
  void setSize(double w, double h);
  void setPos(double x, double y);
  void setText(QString text);
  void setId(int id);
  int id();
  void center(double *x, double *y);

  void setNodeShape(NodeShape shape);
  NodeShape nodeShape();

protected:
  void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *);
  void mousePressEvent(QGraphicsSceneMouseEvent *event);

public slots:
  void selectedNode(int id);

private slots:
  void processClicks();

signals:
  void scopeSelectedDown(int);
  void scopeExpandedDown(int);
  void rightClick(int, QPoint);

private:
  double _x = 0.0, _y = 0.0;
  double _w = 0.0, _h = 0.0;
  QString _text = "";
  int _id = -1;
  bool _selected = false;

  bool _dc = false;
  bool _to = true;
  bool _rc = false;

  QPoint _rcp;

  NodeShape _shape = NodeShape::RECTANGLE;

  void drawRectangle(QPainter *painter);
  void drawTriangle(QPainter *painter);
  void drawHexagon(QPainter *painter);
  void drawEllipse(QPainter *painter);
};

#endif // GSNODE_H
