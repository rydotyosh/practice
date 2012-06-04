
#include <iostream>
#include <map>

using namespace std;

typedef long long ll;
typedef map<ll,ll> ml;
ml mp;

ll coll(ll n, ll cnt)
{
	ml::iterator f = mp.find(n);
	if(f!=mp.end())
		return cnt+f->second;
	if(n&1)
	{
		ll r = coll(3*n+1,cnt+1);
		mp[n] = r-cnt;
		return r;
	}
	else
	{
		ll r = coll(n>>1,cnt+1);
		mp[n] = r-cnt;
		return r;
	}
}

int main()
{
	mp[1]=1;
	/*for(ml::iterator i=mp.begin();i!=mp.end();++i)
	{
		cout<<i->first<<" "<<i->second<<endl;
	}*/
	ll maxv=1;
	ll maxi=1;
	for(ll i=1;i<1000000;++i)
	{
		ll v=coll(i,0);
		if(v>maxv)
		{
			maxv=v;
			maxi=i;
		}
	}
	cout<<maxi<<" "<<maxv<<endl;
	return 0;
}


