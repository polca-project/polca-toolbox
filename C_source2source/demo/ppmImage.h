
#ifndef PPMIMAGE_H
#define PPMIMAGE_H

typedef struct
{
	char *pixel;
	int width, height;
}Image;


void LoadPPMImage (Image *image,const char* imageName);
void SavePPMImage (Image image, const char* imageName);

void LoadPPMImageArray(char **image,int *width,int *height,const char* imageName);
void SavePPMImageArray(char *image,int width,int height, const char* imageName);

//PPMIMAGE_H
#endif 
