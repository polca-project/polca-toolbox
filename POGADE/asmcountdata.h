#ifndef ASMCOUNTDATA_H
#define ASMCOUNTDATA_H

#include <QJsonArray>
#include <QJsonValue>
#include <QJsonObject>
#include <QString>
#include <vector>

typedef struct {
  int id;
  QString name;
  int weight;
  int lstart;
  int lend;
  std::vector<int> scopes;
} ASMCData;

std::vector<ASMCData> getScopes(QJsonArray scopes, int *nextId, std::vector<int> *child);

#endif // ASMCOUNTDATA_H
