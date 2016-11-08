#include "pogadeproject.h"

#include <iostream>
#include <QDebug>
#include <QObject>
#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonValue>
#include <QJsonArray>
#include <QFile>
#include <quazip/JlCompress.h>

PogadeProject::PogadeProject() {
  _tDir.setAutoRemove(true);

  if(!_tDir.isValid()) {
    std::cerr << QObject::tr("No valid temporal directory").toUtf8().constData() << std::endl;
    _valid = false;
  }

#ifdef QT_DEBUG
  qDebug() << "Temporal Directory: " << _tDir.path();
#endif
}

PogadeProject::~PogadeProject() {
  for(int i=0; i<_codeFiles.size(); ++i) {
    delete _codeFiles.at(i);
  }
}

bool PogadeProject::valid() {
  return _valid;
}

void PogadeProject::setName(QString newName) {
  _pName = newName;
}

QString PogadeProject::name() {
  return _pName;
}

void PogadeProject::setFileName(QString newFileName) {
  _fName = newFileName;
}

QString PogadeProject::fileName() {
  return _fName;
}

QString PogadeProject::workingDir() {
  return _tDir.path();
}

QString PogadeProject::path() {
  return _fDir.path();
}

void PogadeProject::create(QString projectName, QString path, QString dirName) {
  _pName = projectName;
  _fDir.setPath(path);
  _fDir.mkdir(dirName);
  _fDir.setPath(_fDir.path() + "/" + dirName);
  _fDir.mkdir("src");
  _fDir.mkdir("rev");
  _fName = _fDir.path() + "/" + projectName + ".pof";
  _lastId = 0;

  saveConfFile();
}

void PogadeProject::generateCodeRevision(QString file, int orgRev, int nextRev) {
  QString orgF;
  QString destF;

  if(orgRev < 0) {
    orgF = _fDir.path() + "/src/" + file;
  } else {
    orgF = _fDir.path() + "/rev/" + file + "_r" + QString::number(orgRev);
  }

  destF = _fDir.path() + "/rev/" + file + "_r" + QString::number(nextRev);

  QFile f(orgF);
  f.copy(destF);

  qDebug() << "FROM: " << orgF;
  qDebug() << "TO  : " << destF;
}

void PogadeProject::openProjectFile(QString file) {
  _fName = file;
  QFileInfo fi(_fName);
  _fDir.setPath(fi.path());
  QFile f(_fName);
  f.open(QIODevice::ReadOnly | QIODevice::Text);
  QJsonDocument doc = QJsonDocument::fromJson(f.readAll());
  f.close();

  if(!doc.isEmpty()) {
    QJsonObject obj = doc.object();
    _pName = obj.value("ProjectName").toString();
    qDebug() << "Project Name: " << _pName;

    _lastId = obj.value("LastID").toInt();

    QJsonArray cf = obj.value("CodeFiles").toArray();
    for(int i = 0; i < cf.size(); ++i) {
      QJsonObject _obj = cf.at(i).toObject();
      PogadeProjectSourceFile *_ppsf = new PogadeProjectSourceFile(this, _obj);

      _codeFiles << _ppsf;
    }
  }

  qDebug() << "Open Project from File: " << _fName;

  //JlCompress::extractDir(_fName, _tDir.path());
  //loadConf();
}

void PogadeProject::addSourceFile(QString file) {
  qDebug() << "Adding file to project: " << file;
  QFile f(file);
  QFileInfo fi(f);

  PogadeProjectSourceFile* _ppsf = new PogadeProjectSourceFile(this, fi.fileName(), _lastId);
  _lastId++;

  if(f.copy(_fDir.path() + "/src/" + fi.fileName())){
    _codeFiles << _ppsf;
      qDebug() << "Added " << fi.fileName();
  } else {
    std::cerr << "File " << fi.fileName().toUtf8().constData();
    std::cerr << " could not be copied and added" << std::endl;
  }

  if(f.copy(_fDir.path() + "/rev/" + fi.fileName() + "_r0")){
      qDebug() << "Added " << fi.fileName() + "_r0 to revisions";
  } else {
    std::cerr << "File " << fi.fileName().toUtf8().constData() << "_r0";
    std::cerr << " could not be copied and added" << std::endl;
  }
}

void PogadeProject::removeFile(unsigned int id) {
  for(int i=0; i<_codeFiles.size(); ++i) {
    PogadeProjectSourceFile* ppsf = _codeFiles.at(i);

    if(ppsf->id() == id) {
      QString _f(_fDir.path() + "/src/" + ppsf->name());
      qDebug() << "Removing file :" << _f;
      QFile::remove(_f);

      for(int j=0; j < ppsf->revisions().size(); ++j) {
        int _r = ppsf->revisions().at(j);
        _f = _fDir.path() + "/rev/" + ppsf->name() + "_r" + QString::number(_r);
        qDebug() << "Removing file :" << _f;
        QFile::remove(_f);
      }

      _codeFiles.removeAt(i);
      delete ppsf;
      break;
    }
  }
}

QList<PogadeProjectSourceFile *> PogadeProject::codeFiles() {
  return _codeFiles;
}

void PogadeProject::saveConfFile() {
  QJsonDocument doc;
  QJsonObject obj;
  QJsonArray _cfs;

  for(int i=0; i<_codeFiles.size(); ++i) {
    PogadeProjectSourceFile *file = _codeFiles.at(i);
    _cfs.append(file->toJson());
  }

  obj.insert("ProjectName", QJsonValue(_pName));
  obj.insert("CodeFiles", _cfs);
  obj.insert("LastID", (int)_lastId);
  doc.setObject(obj);


  QFile f(_fName);
  f.open(QIODevice::WriteOnly);
  f.write(doc.toJson());
  f.close();
}

void PogadeProject::exportProject(QString file) {
  qDebug() << "Export Project to file: " << file;
  JlCompress::compressDir(file, _fDir.path(), true);
}

PogadeProjectSourceFile* PogadeProject::getSourceFile(unsigned int id) {
  for(int i=0; i<_codeFiles.size(); ++i) {
    PogadeProjectSourceFile *file = _codeFiles.at(i);
    if(file->id() == id)
      return file;
  }

  return NULL;
}

QTemporaryDir *PogadeProject::getTDir() {
  return &_tDir;
}
