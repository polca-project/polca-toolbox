#ifndef POGADEREVISIONCREATE_H
#define POGADEREVISIONCREATE_H

#include <QDialog>

namespace Ui {
class PogadeRevisionCreate;
}

class PogadeRevisionCreate : public QDialog
{
  Q_OBJECT

public:
  explicit PogadeRevisionCreate(QWidget *parent = 0);
  ~PogadeRevisionCreate();
  void setFileName(QString name);
  void setNextRevision(unsigned int rev);

private slots:
  void createRevision();

signals:
  void newRevision(QString);

private:
  Ui::PogadeRevisionCreate *ui;
  QString _name;
  unsigned int _rev;
};

#endif // POGADEREVISIONCREATE_H
