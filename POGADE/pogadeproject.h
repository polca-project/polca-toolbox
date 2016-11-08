#ifndef POGADEPROJECT_H
#define POGADEPROJECT_H

#include <QDir>
#include <QString>
#include <QList>
#include <QTemporaryDir>

#include "pogadeprojectsourcefile.h"

/*
 * A Pogade Project consists of:
 * - Name of the project
 * - File (container) of the project
 * - Code files
 *
 * It also uses
 * - temporal directory
 * - info file
 */

class PogadeProject {
public:
  PogadeProject();
  ~PogadeProject();
  bool valid();
  void setName(QString newName);
  QString name();
  void setFileName(QString newFileName);
  QString fileName();
  QString path();
  QString workingDir();
  void create(QString projectName, QString path, QString dirName);
  void openProjectFile(QString file);
  void addSourceFile(QString file);
  void removeFile(unsigned int id);
  QList<PogadeProjectSourceFile*> codeFiles();
  void saveConfFile();
  void exportProject(QString file);
  PogadeProjectSourceFile* getSourceFile(unsigned int id);
  void generateCodeRevision(QString file, int orgRev, int nextRev);
  QTemporaryDir *getTDir();

private:
  QTemporaryDir _tDir;
  QDir _fDir;
  QString _fName = "";
  QString _pName = "";
  bool _valid    = true;
  QList<PogadeProjectSourceFile*> _codeFiles;
  unsigned int _lastId;
};

#endif // POGADEPROJECT_H
