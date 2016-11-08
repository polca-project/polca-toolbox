#include "polcapragma.h"

PolcaPragma::PolcaPragma() {

}

PolcaPragma::~PolcaPragma() {

}

int PolcaPragma::lineStart() {
  return _lineStart;
}

int PolcaPragma::lineEnd() {
  return _lineEnd;
}

QString PolcaPragma::text() {
  return _text;
}

void PolcaPragma::setLineStart(int line) {
  _lineStart = line;
}

void PolcaPragma::setLineEnd(int line) {
  _lineEnd = line;
}

void PolcaPragma::setText(QString text) {
  _text = text;
}
