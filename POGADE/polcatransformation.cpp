#include "polcatransformation.h"

int PolcaTransformation::_idCount = 0;

PolcaTransformation::PolcaTransformation()
{

}

PolcaTransformation::~PolcaTransformation()
{

}

void PolcaTransformation::setId(int id) {
  _id = id;
}

int PolcaTransformation::id() {
  return _id;
}

int PolcaTransformation::idNext() {
  return _idCount;
}

void PolcaTransformation::idNextReset() {
  _idCount = 0;
}

void PolcaTransformation::idNextIncrease() {
  _idCount++;
}

void PolcaTransformation::setTransformationId(int id) {
  _transformationId = id;
}

int PolcaTransformation::transformationId() {
  return _transformationId;
}

void PolcaTransformation::setLineStart(int line) {
  _lineStart = line;
}

int PolcaTransformation::lineStart() {
  return _lineStart;
}

void PolcaTransformation::setCodeNew(QString code) {
  _codeNew = code;
}

QString PolcaTransformation::codeNew() {
  return _codeNew;
}

void PolcaTransformation::setCodeOld(QString code) {
  _codeOld = code;
}

QString PolcaTransformation::codeOld() {
  return _codeOld;
}

void PolcaTransformation::setRuleName(QString name) {
  _ruleName = name;
}

QString PolcaTransformation::ruleName() {
  return _ruleName;
}
