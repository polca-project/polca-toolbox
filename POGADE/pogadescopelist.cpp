#include "pogadescopelist.h"
#include "ui_pogadescopelist.h"

#include "polcascope.h"

#include <vector>
#include <queue>

PogadeScopeList::PogadeScopeList(QWidget *parent) :
  QWidget(parent),
  ui(new Ui::PogadeScopeList)
{
  ui->setupUi(this);
  connect(ui->tree, SIGNAL(currentItemChanged(QTreeWidgetItem*,QTreeWidgetItem*)),
          this, SLOT(processSelectionChanged(QTreeWidgetItem*,QTreeWidgetItem*)));
  updateGUI();
}

PogadeScopeList::~PogadeScopeList()
{
  delete ui;
}

void PogadeScopeList::setSourceFile(PogadeProjectSourceFile *sf) {
  _sf = sf;
  updateGUI();
}

void PogadeScopeList::updateGUI() {
  if(_sf) {
    bool showmem = _sf->memoryShow();

    std::queue<int> toVisitId;
    std::queue<QTreeWidgetItem *> toVisitItem;

    ui->tree->clear();
    std::vector<int> roots = _sf->rootScopes();
    for(int r : roots) {
      bool show = true;
      PolcaScope *s = _sf->findScope(r);

      if(!showmem && (s->getType() == POLCA_MEMALLOC || s->getType() == POLCA_MEMFREE))
        show = false;

      if(show) {
        QTreeWidgetItem *treeItem = new QTreeWidgetItem(ui->tree);
        //treeItem->setData(0, 1, QVariant(s->id()));
        treeItem->setText(0, QString::number(s->id()));
        treeItem->setText(1, s->name());
        treeItem->setText(2, s->pragmaTextAll());

        toVisitId.push(r);
        toVisitItem.push(treeItem);
      }
    }
    while(!toVisitId.empty()) {
      int nodeNow = toVisitId.front();
      toVisitId.pop();
      QTreeWidgetItem * treeNow = toVisitItem.front();
      toVisitItem.pop();

      PolcaScope *s = _sf->findScope(nodeNow);
      treeNow->setText(0, QString::number(s->id()));
      treeNow->setText(1, s->name());
      treeNow->setText(2, s->pragmaTextAll());

      std::vector<ScopeChild> children = s->children();
      for(ScopeChild c : children) {
        QTreeWidgetItem *treeItem = new QTreeWidgetItem();
        toVisitId.push(c.cid);
        toVisitItem.push(treeItem);
        treeNow->addChild(treeItem);
      }
    }

  }
  else {
    ui->tree->clear();
  }
}

void PogadeScopeList::processSelectionChanged(QTreeWidgetItem *current, QTreeWidgetItem *) {
  emit scopeSelectedDown(current->text(0).toInt());
}

void PogadeScopeList::scopeSelectedUpProcess(int id) {
  QTreeWidgetItemIterator it(ui->tree);
  while (*it) {
    if ((*it)->text(0) == QString::number(id)) {
      (*it)->setSelected(true);
    }
    else {
      (*it)->setSelected(false);
    }
    ++it;
  }
}
