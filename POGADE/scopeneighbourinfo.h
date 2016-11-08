#ifndef ScopeNeighbourInfo_H
#define ScopeNeighbourInfo_H

#include <vector>

class ScopeNeighbourInfo
{
public:
  ScopeNeighbourInfo();
  ~ScopeNeighbourInfo();

  void setId(int id);
  int id();
  std::vector<int> inChildren();
  void addInChildren(int id);
  void clearInChildren();
  std::vector<int> outChildren();
  void addOutChildren(int id);
  void clearOutChildren();

private:
  std::vector<int> _in;
  std::vector<int> _out;
  int _id = -1;
};

#endif // ScopeNeighbourInfo_H
