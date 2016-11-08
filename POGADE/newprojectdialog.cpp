#include "newprojectdialog.h"
#include "ui_newprojectdialog.h"

#include <QFileDialog>
#include <QDebug>
#include <iostream>

NewProjectDialog::NewProjectDialog(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::NewProjectDialog)
{
  ui->setupUi(this);

  connect(ui->buttonDirectory, SIGNAL(released()), this, SLOT(selectDirectory()));
}

NewProjectDialog::~NewProjectDialog()
{
  delete ui;
}

void NewProjectDialog::createProject() {
  PogadeProject *newProject = new PogadeProject();
  //TODO: check project is valid

  //TODO: check for valid input
  newProject->create(ui->lineEditName->text(),
                     ui->lineEditDirectory->text(),
                     ui->lineEditDirName->text());


  if(newProject->valid()) {
    emit newProjectCreated(newProject);
    this->accept();
  } else {
    delete newProject;
    std::cerr << QObject::tr("New POLCA Project could not be created").toUtf8().constData() << std::endl;
    //TODO: something else or just show error message
  }
}

void NewProjectDialog::selectDirectory() {
  QString dir = QFileDialog::getExistingDirectory(this, tr("Open Directory"),
                                               "/home",
                                               QFileDialog::ShowDirsOnly
                                               | QFileDialog::DontResolveSymlinks);
  ui->lineEditDirectory->setText(dir);
}
