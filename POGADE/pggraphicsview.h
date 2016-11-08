#ifndef PGGRAPHICSVIEW_H
#define PGGRAPHICSVIEW_H

#include <QGraphicsView>
#include <QWheelEvent>

namespace Ui {
  class PGGraphicsView;
}

class PGGraphicsView : public QGraphicsView
{
  Q_OBJECT

public:
  PGGraphicsView ( QWidget * parent = 0 );
  PGGraphicsView ( QGraphicsScene * scene, QWidget * parent = 0 );
  ~PGGraphicsView();

public slots:
  void zoomPlus();
  void zoomMinus();
  void zoomReset();

protected:
  virtual void wheelEvent ( QWheelEvent * event );

private:
  float _zoom = 1.0f;
};

#endif // PGGRAPHICSVIEW_H
