#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ppmImage.h"

void LoadPPMImage (Image *image,const char* imageName)
{
 char line[255];
 
 FILE *fp = fopen(imageName, "r");
 if (fp != NULL) {
   // read PPM header

   // read P6
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
   /* printf("%s %c\n",line,line[0]); */
   if(strcmp(line,"P6\n")) exit(1);

   // Read comment line
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
     /* printf("%s\n",line); */

     // Read image dimensions "width height"
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
   sscanf(line,"%d %d",&(image->width),&(image->height));
   /* sscanf(line,"%d %d",&width,&height); */
   /* printf("%s -> %d %d\n",line,image->width,image->height); */

   // Read pixel level value
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
   int pixelLevel;
   sscanf(line,"%d",&pixelLevel);
   /* printf("%s -> %d\n",line,pixelLevel); */
   
   // Read image pixels
   // Image is RGB so size is: width * height * 3
   int lSize = image->width * image->height * 3;//18;
   image->pixel = (char*) malloc( sizeof(char) * lSize);
  int result = fread (image->pixel,sizeof(char),lSize,fp);


    fclose (fp);
}
}

void SavePPMImage (Image image, const char* imageName)
{
  char line[255];

  FILE *fp = fopen(imageName, "wb");
  if (fp != NULL) {
    fputs ("P6\n",fp);

    sprintf(line, "%d %d\n",image.width,image.height);
    fputs (line,fp);

    fputs ("255\n",fp);

    fwrite(image.pixel, sizeof(char),image.width*image.height*3, fp);

    fclose (fp);
  }
}

void LoadPPMImageArray(char **image,int *width,int *height,const char* imageName)
{
 char line[255];
 
 FILE *fp = fopen(imageName, "r");
 if (fp != NULL) {
   // read PPM header

   // read P6
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
   /* printf("%s %c\n",line,line[0]); */
   if(strcmp(line,"P6\n")) exit(1);

   // Read comment line
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
     /* printf("%s\n",line); */

     // Read image dimensions "width height"
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
   sscanf(line,"%d %d",width,height);
   /* sscanf(line,"%d %d",&width,&height); */
   /* printf("%s -> %d %d\n",line,image->width,image->height); */

   // Read pixel level value
   if(!fgets(line , 255 , fp)){printf("ERROR: couldn't read line from image file %s\n",imageName);exit(1);}
   int pixelLevel;
   sscanf(line,"%d",&pixelLevel);
   /* printf("%s -> %d\n",line,pixelLevel); */
   
   // Read image pixels
   // Image is RGB so size is: width * height * 3
   int lSize = (*width) * (*height) * 3;//18;
   *image = (char*) malloc( sizeof(char) * lSize);
   int result = fread (*image,sizeof(char),lSize,fp);


    fclose (fp);
}
}

void SavePPMImageArray(char *image,int width,int height, const char* imageName)
{
  char line[255];

  FILE *fp = fopen(imageName, "wb");
  if (fp != NULL) {
    fputs ("P6\n",fp);

    sprintf(line, "%d %d\n",width,height);
    fputs (line,fp);

    fputs ("255\n",fp);

    fwrite(image, sizeof(char),width*height*3, fp);

    fclose (fp);
  }
}
