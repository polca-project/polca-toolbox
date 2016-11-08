#include "pogadeimport.h"
#include "ui_pogadeimport.h"

#include <QFileDialog>
#include <quazip/JlCompress.h>
#include <QFile>
#include <QDir>
#include <QDebug>

PogadeImport::PogadeImport(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::PogadeImport)
{
  ui->setupUi(this);
  ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
}

PogadeImport::~PogadeImport()
{
  delete ui;
}

void PogadeImport::extractProject() {
  QFile f(ui->lineFile->text());
  QFileInfo finfo(f);

  QDir d(ui->lineDirectory->text() + QDir::separator() + finfo.baseName());
  d.mkpath(ui->lineDirectory->text() + QDir::separator() + finfo.baseName());
  JlCompress::extractDir(ui->lineFile->text(), d.absolutePath());
  this->accept();
}

void PogadeImport::selectFile() {
  QString file = QFileDialog::getOpenFileName(this, tr("Open Project File"),
                                              QDir::homePath(), tr("Pogade Project (*.pox)"));

  ui->lineFile->setText(file);
}

void PogadeImport::selectDir() {
  QString dir = QFileDialog::getExistingDirectory(this, tr("Open Directory"),
                                               QDir::homePath(),
                                               QFileDialog::ShowDirsOnly
                                               | QFileDialog::DontResolveSymlinks);
  ui->lineDirectory->setText(dir);
}

void PogadeImport::checkInputData() {
  QFile f(ui->lineFile->text());

  if(f.exists() && ui->lineDirectory->text() != "") {
    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(true);
  }
  else {
    ui->buttonBox->button(QDialogButtonBox::Ok)->setEnabled(false);
  }
}
