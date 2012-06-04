
// g++ 385_3.cpp -lgmpxx -lgmp

#include <vector>
#include <iostream>
#include <map>
#include <algorithm>

#include <gmpxx.h>

using namespace std;

//typedef long long ll;
typedef mpz_class ll;
typedef pair<ll, ll> ll2;

ll isqrt(const ll n)
{
	if(n == 0)return 0;
	if(n == 1)return 1;
	//ll x = n >> 1;
	ll x = sqrt(n);
	for(;x * x > n;)
	{
		x = (x + n / x) >> 1;
	}
	return x;
}

bool issqrt(const ll n, ll &res)
{
	res = isqrt(n);
	return res * res == n;
}

ll gcd(ll a, ll b){return (b != 0) ? gcd(b, a % b) : a;}

ll2 reduct(const ll a, const ll b)
{
	ll g = gcd(a, b);
	return make_pair(a / g, b / g);
}

bool betaratio(const ll b2, ll2 &res)
{
	ll2 r = reduct(3*b2, b2-13);
	if(!issqrt(r.first, res.first)) return false;
	if(!issqrt(r.second, res.second)) return false;
	return true;
}

vector<ll2> genpts(const ll &b2, const ll2 &rat)
{
	vector<ll2> res;
	ll ymax=4*(b2-13);
	for(ll y = 0;; ++y)
	{
		ll y2 = y * y;
		if(y2 > ymax) break;
		ll v1 = y2*b2;
		ll v2 = b2-13;
		if(v1%v2 != 0) continue;
		ll x2 = 4*b2 - v1/v2;
		ll xx;
		if(!issqrt(x2, xx)) continue;
		ll yy;
		if(!issqrt(y2, yy)) continue;
		res.push_back(make_pair(xx, yy));
	}
	return res;
}

/*
-- major_radius to integral points
genAs :: (Integer, a) -> [(Integer, a, (Integer, Integer))]
genAs (b2, rat) = catMaybes (map (bj.bothsqrt.fx2) y2s) where
	ymx = 4*(b2-13)
	y2s = takeWhile (<=ymx) [y*y|y<-[0..]]
	fx2 y2 = ((4*b2-) <$> ((y2*b2)`isdiv`(b2-13)), Just y2)
	bothsqrt (Just x2, Just y2) = (issqrt x2, issqrt y2)
	bothsqrt (_,_) = (Nothing, Nothing)
	bj (Just x, Just y) = Just(b2,rat, (x,y))
	bj (_,_) = Nothing
*/

bool rotpoint(const int sgn, const ll2 p, const ll2 ratio, ll2 &res)
{
	const ll x = p.first;
	const ll y = p.second;
	const ll m = ratio.first;
	const ll k = ratio.second;
	const ll px1 = -k*x -sgn*m*y;
	const ll px2 = 2*k;
	if(px1%px2!=0) return false;
	const ll py1 = sgn*3*k*x -m*y;
	const ll py2 = 2*m;
	if(py1%py2!=0) return false;

	res.first = px1/px2;
	res.second = py1/py2;
	return true;
}

bool otherpoints(const ll2 p, const ll2 ratio, ll2 &p1, ll2 &p2)
{
	if(!rotpoint(1, p, ratio, p1)) return false;
	if(!rotpoint(-1, p, ratio, p2)) return false;
	return true;
}

bool isinside(const ll n, const ll2 p)
{
	return abs(p.first)<=n && abs(p.second)<=n;
}

struct gens
{
	ll x1;
	ll x2;
	ll r1;
	ll r2;
	bool isvalid1;
	bool isvalid2;
	gens():x1(1), x2(2), isvalid1(false), isvalid2(false){}
	ll f1(const ll &x){return 3*x*x+13;}
	ll f2(const ll &x){return 13*x*x;}
	void gen1(){if(!isvalid1) r1=f1(x1); isvalid1=true;}
	void gen2(){if(!isvalid2) r2=f2(x2); isvalid2=true;}
	void next1(){++x1; isvalid1=false;}
	void next2(){++x2; isvalid2=false;}
};

bool operator<(const ll2 &a, const ll2 &b)
{
	if(a.first<b.first)return true;
	if(a.first>b.first)return false;
	if(a.second<b.second)return true;
	return false;
}

bool operator ==(const ll2 &a, const ll2 &b)
{
	return (a.first == b.first) && (a.second == b.second);
}

ll2 negx(const ll2 &p){return make_pair(-p.first, p.second);}
ll2 negy(const ll2 &p){return make_pair(p.first, -p.second);}
ll2 negboth(const ll2 &p){return make_pair(-p.first, -p.second);}

struct triangle
{
	vector<ll2> pts;
	triangle(){pts.resize(3);}
	triangle(const ll2 &a, const ll2 &b, const ll2 &c)
	{
		pts.resize(3);
		pts[0]=a;
		pts[1]=b;
		pts[2]=c;
	}
	bool operator<(const triangle &t)const
	{
		if(pts[0] < t.pts[0])return true;
		if(pts[0] != t.pts[0]) return false;
		if(pts[1] < t.pts[1])return true;
		if(pts[1] != t.pts[1]) return false;
		if(pts[2] < t.pts[2])return true;
		return false;
	}
	bool operator==(const triangle &t)const
	{
		if(!(pts[0]==t.pts[0])) return false;
		if(!(pts[1]==t.pts[1])) return false;
		if(!(pts[2]==t.pts[2])) return false;
		return true;
	}
	void sort()
	{
		std::sort(pts.begin(), pts.end());
	}

	void mirrors(vector<triangle> &res)const
	{
		res.resize(4);
		res[0] = *this;
		for(size_t i=0;i<3;++i)
		{
			res[1].pts[i] = negx(pts[i]);
			res[2].pts[i] = negy(pts[i]);
			res[3].pts[i] = negboth(pts[i]);
		}
		res[1].sort();
		res[2].sort();
		res[3].sort();
		std::sort(res.begin(), res.end());
		res.erase(unique(res.begin(), res.end()), res.end());
	}

	ll area2()const
	{
		ll dx1 = pts[1].first - pts[0].first;
		ll dy1 = pts[1].second - pts[0].second;
		ll dx2 = pts[2].first - pts[0].first;
		ll dy2 = pts[2].second - pts[0].second;
		return abs(dx1*dy2 - dy1*dx2);
	}
};

ll processtri(const ll &n, const ll &b2, const ll2 &rat, const vector<ll2> &pts)
{
	vector<triangle> mrs;
	for(size_t i=0;i<pts.size();++i)
	{
		const ll2 &p = pts[i];
		ll2 p1, p2;
		if(!otherpoints(p, rat, p1, p2)) continue;
		if(!isinside(n, p1)) continue;
		if(!isinside(n, p2)) continue;

		triangle t(p, p1, p2);
		t.sort();
		vector<triangle> ms;
		t.mirrors(ms);
		copy(ms.begin(), ms.end(), back_inserter(mrs));
	}

	sort(mrs.begin(), mrs.end());
	vector<triangle>::iterator e = unique(mrs.begin(), mrs.end());

	ll a(0);
	for(vector<triangle>::iterator i=mrs.begin();i!=e;++i)
	{
		a+=i->area2();
	}

	return a;
}

int main()
{
	ll n=1000000000;
	ll mxr=n*n;

	ll sma(0);

	long ntrial = 0;
	long nratio = 0;
	long nprocess = 0;

	gens g;
	bool v1end = false;
	bool v2end = false;
	ll prevb2(0);
	for(;;)
	{
		++ntrial;

		ll &v1=g.r1;
		ll &v2=g.r2;
		if(!v1end)
		{
			g.gen1();
			if(mxr<v1) v1end=true;
		}
		if(!v2end)
		{
			g.gen2();
			if(mxr<v2) v2end=true;
		}
		if(v1end && v2end) break;

		ll b2(0);

		if(!v1end && !v2end)
		{
			if(v1<v2)
			{
				b2=v1;
				g.next1();
			}
			else
			{
				b2=v2;
				g.next2();
			}
		}
		else if(v1end)
		{
			b2=v2;
			g.next2();
		}
		else if(v2end)
		{
			b2=v1;
			g.next1();
		}

		if(b2==prevb2)
			continue;
		prevb2 = b2;

		ll2 rat;
		if(!betaratio(b2, rat)) continue;

		++nratio;

		const vector<ll2> pts = genpts(b2, rat);
		if(pts.empty()) continue;

		vector<ll2> qs;
		for(size_t i=0;i<pts.size();++i)
		{
			const ll2 &p = pts[i];
			if(!isinside(n, p)) continue;
			qs.push_back(p);
		}

		sma+=processtri(n, b2, rat, qs);
		++nprocess;
	}

	cout<<sma/2<<endl;

	cout<<endl;
	cout<<ntrial<<endl;
	cout<<nratio<<endl;
	cout<<nprocess<<endl;
	return 0;
}


