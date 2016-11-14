#include "pogadesettings.h"
#include "ui_pogadesettings.h"

#include <QSettings>
#include <QFileDialog>
#include <QDebug>

#include "pogadedefines.h"

pogadeSettings::pogadeSettings(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::pogadeSettings)
{
  QSettings settings;

  ui->setupUi(this);

  // Load Settings
  ui->linePR->setText(settings.value("PTReader", POLCATOOLREADER).toString());
  //ui->linePP->setText(settings.value("PTPretty", POLCATOOLPRETTY).toString());

  // Load Language Settings
  ui->comboLanguage->addItem(tr("English"), QVariant(QLocale::English));
  ui->comboLanguage->addItem(tr("Spanish"), QVariant(QLocale::Spanish));

  // Set index to current language
  int lang = settings.value("language", QLocale().language()).toInt();
  int index = ui->comboLanguage->findData(QVariant(lang));
  ui->comboLanguage->setCurrentIndex(index);
}

pogadeSettings::~pogadeSettings() {
  delete ui;
}

void pogadeSettings::chooseToolPR() {
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca Reader Tool Binary"), "");

  if(!fileName.isEmpty()) {
    ui->linePR->setText(fileName);
  }
}

/*
void pogadeSettings::chooseToolPP() {
  auto fileName = QFileDialog::getOpenFileName(this,
    tr("Open Polca Pretty Tool Binary"), "");

  if(!fileName.isEmpty()) {
    ui->linePP->setText(fileName);
  }
}
*/

void pogadeSettings::doApply() {
  // TODO: check tools after setting new ones
  qDebug() << "saving Setings "  << ui->comboLanguage->currentData().toInt();

  QSettings settings;
  settings.setValue("PTReader", ui->linePR->text());
  //settings.setValue("PTPretty", ui->linePP->text());

  // Save Language Settings
  settings.setValue("language", ui->comboLanguage->currentData().toInt());

  // TODO: change langue in the whole application without restarting
  //qDebug() << "Saved Language";
}

void pogadeSettings::doOK() {
  this->doApply();
  this->close();
  this->~pogadeSettings();
}

void pogadeSettings::doCancel() {
  this->close();
  this->~pogadeSettings();
}
