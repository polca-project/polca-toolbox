#ifndef LIBEMOXU3_H
#define LIBEMOXU3_H

#include <string>
#include <sys/ioctl.h>

#define GPUFREQ_NODE    "/sys/devices/11800000.mali/clock"
#define TEMP_NODE       "/sys/devices/10060000.tmu/temp"

using namespace std;

typedef struct ina231_iocreg__t {
    char name[20];
    unsigned int enable;
    unsigned int cur_uV;
    unsigned int cur_uA;
    unsigned int cur_uW;
} ina231_iocreg_t;

typedef struct sensor__t {
    int  fd;
    ina231_iocreg_t data;
} sensor_t;

#define INA231_IOCGREG      _IOR('i', 1, ina231_iocreg_t *)
#define INA231_IOCSSTATUS   _IOW('i', 2, ina231_iocreg_t *)
#define INA231_IOCGSTATUS   _IOR('i', 3, ina231_iocreg_t *)

#define DEV_SENSOR_ARM  "/dev/sensor_arm"
#define DEV_SENSOR_MEM  "/dev/sensor_mem"
#define DEV_SENSOR_KFC  "/dev/sensor_kfc"
#define DEV_SENSOR_G3D  "/dev/sensor_g3d"

enum    {
    SENSOR_ARM = 0,
    SENSOR_MEM,
    SENSOR_KFC,
    SENSOR_G3D,
    SENSOR_MAX
};


class GetNode
{
public:
    GetNode();
    string cpu_node_list[8];
    float armuV,armuA, armuW;
    float g3duV, g3duA, g3duW;
    float kfcuV, kfcuA, kfcuW;
    float memuV, memuA, memuW;
    int usage[8]   = {0};
    int cpuFreq[8] = {0};
    int cpuTemp[4] = {0};
    int gpuFreq    = 0;
    int gpuTemp    = 0;

    int GetGPUCurFreq(void);
    int GetCPUCurFreq(int cpuNum);
    int GetOneCPUCurFreq(int cpuNum);
    int GetOneCPUTemp(int cpuNum);
    int GetCPUTemp(int cpuNum);
    int GetGPUTemp();
    int GetCPUUsage(void);
    int calUsage(int cpu_idx, int user, int nice, int system, int idle);

    int open_sensor(const char *node, sensor_t *sensor);
    void close_sensor(sensor_t *sensor);
    void enable_sensor(sensor_t *sensor, unsigned char enable);
    int read_sensor_status(sensor_t *sensor);
    int read_sensor(sensor_t *sensor);

    int OpenINA231(void);
    void CloseINA231(void);
    int GetINA231(void);

private:
    sensor_t sensor[SENSOR_MAX];
    int mOldUserCPU[8];
    int mOldSystemCPU[8];
    int mOldIdleCPU[8];

};

#endif // LIBEMOXU3
