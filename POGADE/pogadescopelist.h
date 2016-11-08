#ifndef POGADESCOPELIST_H
#define POGADESCOPELIST_H

#include <QWidget>
#include <QTreeWidgetItem>
#include "pogadeprojectsourcefile.h"

namespace Ui {
  class PogadeScopeList;
}

class PogadeScopeList : public QWidget
{
  Q_OBJECT

public:
  explicit PogadeScopeList(QWidget *parent = 0);
  ~PogadeScopeList();
  void setSourceFile(PogadeProjectSourceFile *sf);

private slots:
  void processSelectionChanged(QTreeWidgetItem*, QTreeWidgetItem*);

signals:
  void scopeSelectedDown(int);

public slots:
  void updateGUI();
  void scopeSelectedUpProcess(int id);

private:
  Ui::PogadeScopeList *ui;
  PogadeProjectSourceFile *_sf = nullptr;

  void addTreeChild(QTreeWidgetItem *parent, PolcaScope *s);
};

#endif // POGADESCOPELIST_H
