#ifndef TOOLSWAIT_H
#define TOOLSWAIT_H

#include <QDialog>

namespace Ui {
  class toolsWait;
}

class toolsWait : public QDialog
{
  Q_OBJECT

public:
  explicit toolsWait(QWidget *parent = 0);
  ~toolsWait();

public slots:
  void cancelTool();

private:
  Ui::toolsWait *ui;
};

#endif // TOOLSWAIT_H
