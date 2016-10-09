#include "libemoxu3.h"
#include <sys/ioctl.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <string>
#include <fstream>

#include <iostream>


GetNode::GetNode() {
  for (int i = 0; i < 8; i++) {
    string temp("/sys/devices/system/cpu/cpu" + std::to_string(i) + "/cpufreq/cpuinfo_cur_freq");
    cpu_node_list[i] = temp;
  }
}

int GetNode::GetGPUCurFreq() {
  std::ifstream fp;
  fp.open(GPUFREQ_NODE);

  if(fp.fail()) {
    std::cerr << "GPUFRE_NODE open failed" << endl;
    return -1;
  }

  string freq;
  std::getline(fp, freq);
  fp.close();
  gpuFreq = std::stoi(freq);

  return 0;
}

int GetNode::GetOneCPUCurFreq(int cpuNum) {
  std::ifstream fp;
  fp.open(cpu_node_list[cpuNum]);

  if(fp.fail()) {
    std::cerr << "CPU " << cpuNum << " open failed: " << cpu_node_list[cpuNum] << endl;
    return -1;
  }

  string freq;
  std::getline(fp, freq);
  fp.close();
  int freqi = std::stoi(freq)/1000;

  return freqi;
}

int GetNode::GetCPUCurFreq(int cpuNum) {
  if(cpuNum > 7) {
    return -1;
  }
  else if(cpuNum >= 0) {
    cpuFreq[cpuNum] = GetOneCPUCurFreq(cpuNum);
    return 0; 
  }
  else {
    for(int i=0; i<8; i++) {
      cpuFreq[i] = GetOneCPUCurFreq(i);
    }
    return 0;
  }
}

int GetNode::GetOneCPUTemp(int cpuNum) {
  std::ifstream fp;
  fp.open(TEMP_NODE);

  if(fp.fail())
    return -1;

  char buf[16];
  for (int i=0; i<cpuNum+1; i++)
    fp.read(buf, 16);
  fp.close();

  buf[12] = '\0';

  return atoi(&buf[9]);
}

int GetNode::GetCPUTemp(int cpuNum) {
  if(cpuNum > 3) {
    return -1;
  }
  else if(cpuNum <0) {
    for(int i=0; i<4; i++) {
      cpuTemp[i] = GetOneCPUTemp(i);
    }
  }
  else {
    cpuTemp[cpuNum] = GetOneCPUTemp(cpuNum);
  }

  return 0;
}

int GetNode::GetGPUTemp() {
  std::ifstream fp;
  fp.open(TEMP_NODE);

  if(fp.fail())
    return -1;

  char buf[16];
  for (int i=0; i<5; i++)
    fp.read(buf, 16);
  fp.close();

  buf[12] = '\0';
  
  gpuTemp = atoi(&buf[9]);

  return 0;
}

int GetNode::open_sensor(const char *node, sensor_t *sensor) {
  if ((sensor->fd = open(node, O_RDWR)) < 0) {}
  //   qDebug() << node << "Open Fail";

  return sensor->fd;
}

/////////////////////////////////////////////////////////////////////////////

int GetNode::OpenINA231() {
  if (open_sensor(DEV_SENSOR_ARM, &sensor[SENSOR_ARM]) < 0)
    return -1;
  if (open_sensor(DEV_SENSOR_MEM, &sensor[SENSOR_MEM]) < 0)
    return -1;
  if (open_sensor(DEV_SENSOR_KFC, &sensor[SENSOR_KFC]) < 0)
    return -1;
  if (open_sensor(DEV_SENSOR_G3D, &sensor[SENSOR_G3D]) < 0)
    return -1;

  if (read_sensor_status(&sensor[SENSOR_ARM]))
    return -1;
  if (read_sensor_status(&sensor[SENSOR_MEM]))
    return -1;
  if (read_sensor_status(&sensor[SENSOR_KFC]))
    return -1;
  if (read_sensor_status(&sensor[SENSOR_G3D]))
    return -1;

  if (!sensor[SENSOR_ARM].data.enable)
    enable_sensor(&sensor[SENSOR_ARM], 1);
  if (!sensor[SENSOR_MEM].data.enable)
    enable_sensor(&sensor[SENSOR_MEM], 1);
  if (!sensor[SENSOR_KFC].data.enable)
    enable_sensor(&sensor[SENSOR_KFC], 1);
  if (!sensor[SENSOR_G3D].data.enable)
    enable_sensor(&sensor[SENSOR_G3D], 1);

  return 0;
}

void GetNode::CloseINA231() {
  if (sensor[SENSOR_ARM].data.enable)
    enable_sensor(&sensor[SENSOR_ARM], 0);
  if (sensor[SENSOR_MEM].data.enable)
    enable_sensor(&sensor[SENSOR_MEM], 0);
  if (sensor[SENSOR_KFC].data.enable)
    enable_sensor(&sensor[SENSOR_KFC], 0);
  if (sensor[SENSOR_G3D].data.enable)
    enable_sensor(&sensor[SENSOR_G3D], 0);

  close_sensor(&sensor[SENSOR_ARM]);
  close_sensor(&sensor[SENSOR_MEM]);
  close_sensor(&sensor[SENSOR_KFC]);
  close_sensor(&sensor[SENSOR_G3D]);
}

int GetNode::GetINA231() {
  int res;

  res =  read_sensor(&sensor[SENSOR_ARM]);
  res += read_sensor(&sensor[SENSOR_MEM]);
  res += read_sensor(&sensor[SENSOR_KFC]);
  res += read_sensor(&sensor[SENSOR_G3D]);

  armuV = (float)(sensor[SENSOR_ARM].data.cur_uV / 100000) / 10;
  armuA = (float)(sensor[SENSOR_ARM].data.cur_uA / 1000) / 1000;
  armuW = (float)(sensor[SENSOR_ARM].data.cur_uW / 1000) / 1000;

  memuV = (float)(sensor[SENSOR_MEM].data.cur_uV / 100000) / 10;
  memuA = (float)(sensor[SENSOR_MEM].data.cur_uA / 1000) / 1000;
  memuW = (float)(sensor[SENSOR_MEM].data.cur_uW / 1000) / 1000;

  kfcuV = (float)(sensor[SENSOR_KFC].data.cur_uV / 100000) / 10;
  kfcuA = (float)(sensor[SENSOR_KFC].data.cur_uA / 1000) / 1000;
  kfcuW = (float)(sensor[SENSOR_KFC].data.cur_uW / 1000) / 1000;

  g3duV = (float)(sensor[SENSOR_G3D].data.cur_uV / 100000) / 10;
  g3duA = (float)(sensor[SENSOR_G3D].data.cur_uA / 1000) / 1000;
  g3duW = (float)(sensor[SENSOR_G3D].data.cur_uW / 1000) / 1000;

  return res;
}

void GetNode::enable_sensor(sensor_t *sensor, unsigned char enable) {
  if (sensor->fd > 0) {
    sensor->data.enable = enable ? 1 : 0;
    if (ioctl(sensor->fd, INA231_IOCSSTATUS, &sensor->data) < 0){}
  }
}

int GetNode::read_sensor_status(sensor_t *sensor) {
  if (sensor->fd > 0) {
    if (ioctl(sensor->fd, INA231_IOCGSTATUS, &sensor->data) < 0) {
      std::cerr << "Read Sensor Status IOCTL Error!" << endl;
      return -1;
    }
  }
  return 0;
}

int GetNode::read_sensor(sensor_t *sensor) {
  if (sensor->fd > 0) {
    if (ioctl(sensor->fd, INA231_IOCGREG, &sensor->data) < 0) {
      std::cerr << "Read Sensor IOCTL Error!" << endl;
      return -1;
    }
  }

  return 0;
}

void GetNode::close_sensor(sensor_t *sensor) {
  if (sensor->fd > 0)
    close(sensor->fd);
}

int GetNode::calUsage(int cpu_idx, int user, int nice, int system, int idle) {
  long total = 0;
  long usage = 0;
  int diff_user, diff_system, diff_idle;

  diff_user = mOldUserCPU[cpu_idx] - user;
  diff_system = mOldSystemCPU[cpu_idx] - system;
  diff_idle = mOldIdleCPU[cpu_idx] - idle;

  total = diff_user + diff_system + diff_idle;

  if (total != 0)
    usage = (diff_user * 100) / total;

  mOldUserCPU[cpu_idx] = user;
  mOldSystemCPU[cpu_idx] = system;
  mOldIdleCPU[cpu_idx] = idle;

  return usage;
}

int GetNode::GetCPUUsage(void) {
  char buf[128] = {0,};
  char cpuid[8] = "cpu";
  int findCPU = 0;
  int user, system, nice, idle;
  FILE *fp;
  int cpu_index = 0;

  fp = fopen("/proc/stat", "r");
  if (fp == NULL)
    return -1;

  while (fgets(buf, 128, fp)) {
    char temp[4] = "cpu";
    temp[3] = '0' + cpu_index;
    if (!strncmp(buf, temp, 4)) {
      findCPU = 1;
      sscanf(buf, "%s %d %d %d %d",
	     cpuid, &user, &nice, &system, &idle);
	    
      usage[cpu_index] = calUsage(cpu_index, user, nice, system, idle);

      cpu_index++;
    }
    if (!strncmp(buf, "intr", 4))
      break;
    if (findCPU == 0) {

    }
    else
      findCPU = 0;
  }
  fclose(fp);

  return 0;
}
