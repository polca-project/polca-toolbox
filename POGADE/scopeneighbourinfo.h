#ifndef ScopeNeighbourInfo_H
#define ScopeNeighbourInfo_H

#include <unordered_set>

class ScopeNeighbourInfo
{
public:
  ScopeNeighbourInfo();
  ~ScopeNeighbourInfo();

  std::unordered_set<int> inNeighbours();
  void addInNeighbours(int id);
  void clearInNeighbours();

  std::unordered_set<int> outNeighbours();
  void addOutNeighbours(int id);
  void clearOutNeighbours();

private:
  std::unordered_set<int> _in;
  std::unordered_set<int> _out;
};

#endif // ScopeNeighbourInfo_H
