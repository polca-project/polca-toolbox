// TEST_VECTOR: [1, 3, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 2, 0]
// TEST_LABEL: 0 (MPI/OpenMP)

void kernelRedFilter(char* image,int i,int rawWidth,char **outputImage){
  char r,g,b;

     for(int j = 0; j < rawWidth; j+=3)
  	{
  	  r = image[i*rawWidth+j];
  	  g = image[i*rawWidth+j+1];
  	  b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = r;
  	  (*outputImage)[i*rawWidth+j+1] = 0;
  	  (*outputImage)[i*rawWidth+j+2] = 0;
  	}
}

void kernelGreenFilter(char* image,int i,int rawWidth,char **outputImage){
  char r,g,b;

     for(int j = 0; j < rawWidth; j+=3)
  	{
  	  r = image[i*rawWidth+j];
  	  g = image[i*rawWidth+j+1];
  	  b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = 0;
  	  (*outputImage)[i*rawWidth+j+1] = g;
  	  (*outputImage)[i*rawWidth+j+2] = 0;
  	}
}

void kernelBlueFilter(char* image,int i,int rawWidth,char **outputImage){
  char r,g,b;

     for(int j = 0; j < rawWidth; j+=3)
  	{
  	  r = image[i*rawWidth+j];
  	  g = image[i*rawWidth+j+1];
  	  b = image[i*rawWidth+j+2];

  	  (*outputImage)[i*rawWidth+j] = 0;
  	  (*outputImage)[i*rawWidth+j+1] = 0;
  	  (*outputImage)[i*rawWidth+j+2] = b;
  	}
}

void rgbImageFilter(char* image,int width,int height,char **redImage,char **greenImage,char **blueImage)
{

  unsigned rawWidth = width * 3;

  printf("Applying red color filter...\n");
  printf("Applying green color filter...\n");
  printf("Applying blue color filter...\n");

  // just to check
  int num_proc = 4;

  for (int rank = 0; rank < num_proc; rank++)
  {
    int prev_chunk_size = height / num_proc;
    int curr_chunk_size = rank != (num_proc-1) ? prev_chunk_size : prev_chunk_size + height%num_proc;

    for(int i=rank*prev_chunk_size;i<((rank+1)*curr_chunk_size);i++)
    {
      kernelRedFilter(image,i,rawWidth,redImage);
      kernelGreenFilter(image,i,rawWidth,greenImage);
      kernelBlueFilter(image,i,rawWidth,blueImage);
    }
  }

}
