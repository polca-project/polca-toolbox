#pragma def is_vc_space(V,[c,v])
#pragma def vc_space(V,F,+,*)
#pragma def is_sc_field(F,[a,b])
#pragma def sc_field(F,float,+,-,*,/)

#define N 10

// map g (zipwith (map f v) v)

// f v = a * v

// g (c, v) = c + (b * v)

int main()
{
	int c[N], v[N], a, b, i;

	a = N;
	b = N;

	#pragma polca def f
	#pragma polca map v c
	for(i=0;i<N;i++)
	{
		c[i] = a*v[i];
	}

	#pragma polca def g
	#pragma polca map zip(c,v) c
	for(i=0;i<N;i++)
	{
		#pragma polca input (c[i], v[i])
		#pragma polca output c[i]
		c[i] += b*v[i];
	}

	return 0;
}
