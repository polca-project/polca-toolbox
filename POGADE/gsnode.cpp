#include "gsnode.h"

#include <QDebug>
#include <QApplication>
#include <QTimer>

//GSNode::GSNode(QObject *parent)
//: QObject(parent)
//{
//  setFlag(ItemIsMovable);
//  setFlag(ItemSendsGeometryChanges);
//  setCacheMode(DeviceCoordinateCache);
//  setZValue(-1);
//}

QRectF GSNode::boundingRect() const
{
  qreal penWidth = 1;

  if(_selected) {
    penWidth = 5;
  }

  // NOTE: penWith? penWith/2 for x and y?
  // NOTE: A filled rectangle has a size of rectangle.size(). A stroked rectangle has a size of rectangle.size() plus the pen width.

  //return QRectF(_x - penWidth, _y - penWidth,
  //              _w + penWidth, _h + penWidth);

  return QRectF(_x-_w/2 - penWidth, _y-_h/2 - penWidth,
                _w + penWidth, _h + penWidth);
}

// FIXME: for now not all parameters are used
void GSNode::paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
                   QWidget *widget)
{
  // NOTE: A filled rectangle has a size of rectangle.size(). A stroked rectangle has a size of rectangle.size() plus the pen width.
  // qDebug() << "NODE: " << _id;
  // qDebug() << _x << _y << _w << _h;

  if(_selected) {
    QPen p = painter->pen();
    p.setWidth(5);
    painter->setPen(p);
  }
  else {
    QPen p = painter->pen();
    p.setWidth(1);
    painter->setPen(p);
  }

  switch(_shape) {
    case NodeShape::RECTANGLE:
      drawRectangle(painter);
      break;
    case NodeShape::TRIANGLE:
      drawTriangle(painter);
      break;
    case NodeShape::HEXAGON:
      drawHexagon(painter);
      break;
    case NodeShape::ELLIPSE:
      drawEllipse(painter);
      break;
    default:
      // No shape shape?
      break;
  }

  if(_selected) {
    QPen p = painter->pen();
    p.setWidth(1);
    painter->setPen(p);
  }
}

void GSNode::drawRectangle(QPainter *painter) {
  painter->drawRect(_x-_w/2, _y-_h/2, _w, _h);
  //painter->drawRect(_x, _y, _w, _h);
  //painter->drawText(_x + 10, _y + 20, Qt::TextWordWrap, _text);
  painter->drawText(QRect(_x-_w/2, _y-_h/2, _w, _h), Qt::TextWordWrap | Qt::AlignCenter, _text);
  //painter->drawText(QRect(_x, _y, _w, _h), Qt::TextWordWrap | Qt::AlignCenter, _text);
}

void GSNode::drawTriangle(QPainter *painter) {
  QPointF points[3] = {
    QPointF(_x, _y+_h),
    QPointF(_x+_w, _y+_h),
    QPointF(_x+(_w/2), _y)
  };
  painter->drawPolygon(points, 3);
  painter->drawText(_x + 10, _y + 20, _text);
}

void GSNode::drawHexagon(QPainter *painter) {
  float w1t = _w/3;
  float w2t = w1t*2;
  QPointF points[6] = {
    QPointF(_x+w1t, _y),
    QPointF(_x+w2t, _y),
    QPointF(_x+_w, _y+(_h/2)),
    QPointF(_x+w2t, _y+_h),
    QPointF(_x+w1t, _y+_h),
    QPointF(_x, _y+(_h/2))
  };
  painter->drawPolygon(points, 6);
  painter->drawText(_x + 10, _y + 20, _text);
}

void GSNode::drawEllipse(QPainter *painter) {
  painter->drawEllipse(_x, _y, _w, _h);
  painter->drawText(_x + 10, _y + 20, _text);
}

void GSNode::center(double *x, double *y) {
  *x = _x + (_w/2);
  *y = _y + (_h/2);
}

void GSNode::setPosSize(double x, double y, double w, double h) {
  setPos(x, y);
  setSize(w, h);
}

void GSNode::setSize(double w, double h) {
  _w = w;
  _h = h;
}

void GSNode::setPos(double x, double y) {
  _x = x;
  _y = y;
}

void GSNode::setText(QString text) {
  _text = text;
}

void GSNode::setId(int id) {
  _id = id;
}

int GSNode::id() {
  return _id;
}

void GSNode::setNodeShape(NodeShape shape) {
  _shape = shape;
}

NodeShape GSNode::nodeShape() {
  return _shape;
}

void GSNode::selectedNode(int id) {
  _selected = (id == _id);
}

void GSNode::mousePressEvent(QGraphicsSceneMouseEvent *event) {
  //event->accept();

  if(event->button() == Qt::LeftButton) {
    _dc = false;
    _rc = false;
    if(_to)
      QTimer::singleShot(QApplication::doubleClickInterval(), this, SLOT(processClicks()));
  }
  else if(event->button() == Qt::RightButton){
    _rc = true;
    _rcp = event->screenPos();
    QTimer::singleShot(QApplication::doubleClickInterval()/100, this, SLOT(processClicks()));
  }
}

void GSNode::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *) {
  //event->accept();
  _dc = true;
}

void GSNode::processClicks() {
  _to = true;
  if(_rc) {
    emit scopeSelectedDown(_id);
    emit rightClick(_id, _rcp);
  }
  if(_dc) {
    emit scopeExpandedDown(_id);
  }
  else {
    emit scopeSelectedDown(_id);
  }
}


