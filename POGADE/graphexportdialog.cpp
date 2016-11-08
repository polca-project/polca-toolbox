#include "graphexportdialog.h"
#include "ui_graphexportdialog.h"

#include <QFileDialog>
#include <QMessageBox>

#include "pogadedefines.h"

GraphExportDialog::GraphExportDialog(VGraph *graph, QWidget *parent):
  QWidget(parent),
  ui(new Ui::GraphExportDialog)
{
  _g = graph;
  ui->setupUi(this);
}

GraphExportDialog::~GraphExportDialog() {
  delete ui;
}

void GraphExportDialog::chooseFile() {
  // TODO: improve file selection;
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca Tool Binary"), "");

  // TODO: put extension according to Export Type selection
  if(!fileName.isEmpty()) {
    ui->lineExportFile->setText(fileName);
  }
}

void GraphExportDialog::exportFile() {
  if(!_g)
    return;

  ExportType type;
  switch(ui->comboFileType->currentIndex()) {
    case 0:
      type = ExportType::GML;
      break;
    case 1:
      type = ExportType::DOT;
      break;
    case 2:
      type = ExportType::SVG;
      break;
    default:
      type = ExportType::GML;
      break;
  }

  QString _file;
  _file = ui->lineExportFile->text();
  bool correctExport = _g->exportGraph(_file.toStdString(), type);

  if(!correctExport) {
    QMessageBox::critical(this, MYNAME, tr("Graph could not be exported"));
  }

  this->close();
}
