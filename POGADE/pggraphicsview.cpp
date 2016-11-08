#include "pggraphicsview.h"

#include <QDebug>

PGGraphicsView::PGGraphicsView ( QWidget * parent )
: QGraphicsView(parent)
{

}

PGGraphicsView::PGGraphicsView ( QGraphicsScene * scene, QWidget * parent)
: QGraphicsView(scene,parent)
{

}

PGGraphicsView::~PGGraphicsView() {

}

void PGGraphicsView ::wheelEvent ( QWheelEvent * event ) {
  if(event->modifiers() & Qt::ControlModifier) {
    int d = event->angleDelta().y();
    if(d>0)
      zoomPlus();
    if(d<0)
      zoomMinus();
  }
  else {
    QGraphicsView::wheelEvent(event);
  }
}

void PGGraphicsView::zoomPlus() {
  _zoom *= 1.2;
  scale(1.2, 1.2);
}

void PGGraphicsView::zoomMinus() {
  _zoom *= 0.8;
  scale(0.8, 0.8);
}

void PGGraphicsView::zoomReset() {
  float r = 1.0/_zoom;
  scale(r, r);
  _zoom = 1.0f;
}
