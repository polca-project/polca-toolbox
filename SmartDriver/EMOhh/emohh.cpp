#include <sys/types.h>
#include <sys/wait.h>
#include <iostream>
#include <unistd.h>
#include <string>
#include <sstream>
#include <fstream>
#include <stdlib.h>
#include <algorithm>

#include "libemohh.h"

using namespace std;

int _executable = 1;
string _executableCommand = "";

const char* getCmdOption(const char ** begin, const char ** end, const std::string & option) {
  auto itr = std::find(begin, end, option);
  std::find(begin, end, option);

  if (itr != end && ++itr != end) {
    return *itr;
  }
  return 0;
}

void parseArguments(int argc, const char* argv[]) {
  const char *_executable1 = getCmdOption(argv, argv + argc, "-e");
  if (_executable1) {
    _executable = 1;
    _executableCommand = _executable1;
  }
  const char *_executable2 = getCmdOption(argv, argv + argc, "--executable");
  if (_executable2) {
    _executable = 1;
    _executableCommand = _executable2;
  }

}

int main(int argc, const char* argv[]) {
  pid_t pid = -1;
  int status;

  parseArguments(argc, argv);

  EHHReset();

  if(_executable) {
    pid = fork();
    if(pid == -1) {
      std::cerr << "Starting executable failed" << std::endl;
      exit(-1);
    }
    else if (pid == 0) {
      /* Child code */
      std::stringstream command(_executableCommand);
      std::string segment;
      std::vector<std::string> argList;

      while(std::getline(command, segment, ' ')){
        argList.push_back(segment);
      }
      if(argList.empty()) {
	std::cerr << "Wrong executable command" << std::endl;
        exit(-1);
      }

      const char **_args = (const char **) malloc(sizeof(char*) * (argList.size()+1));
      const char *_exec  = argList.front().c_str();

      int j = 0;
      for(std::vector<std::string>::size_type i = 0; i != argList.size(); i++) {
        _args[j] = argList[i].c_str();
        j++;
      }
      _args[j] = NULL;

      if(execvp(_exec, (char* const*) _args)) {
	std::cerr << "Error executing the command" << std::endl;
        exit(-1);
      }
    }
  }


  if(_executable) {
    int finished = waitpid(pid, &status, 0);
    if(finished) {
      if(WIFEXITED(status)) {
	
      }
      
      if(WIFSIGNALED(status)) {
	
      }

      if(WCOREDUMP(status)) {
	
      }
    }
  }

  uint64_t ts = EHHGetTime();
  uint64_t en = EHHGetEnergy();

  cout << (ts/1000) << " " << en << endl;

  return 0;
}
