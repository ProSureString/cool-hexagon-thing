

mat3 rotz(float a){ float c=cos(a), s=sin(a); return mat3(c,-s,0,s,c,0,0,0,1); }

float sdHexPrism(vec3 p, vec2 h){
    p=abs(p);
    const vec3 k=vec3(-0.8660254,0.5,0.57735);
    p.xy -= 2.0*min(dot(k.xy,p.xy),0.0)*k.xy;
    vec2 d=vec2(length(p.xy-vec2(clamp(p.x,-k.z*h.x,k.z*h.x),h.x))*sign(p.y-h.x),p.z-h.y);
    return min(max(d.x,d.y),0.0)+length(max(d,0.0));
}

#define ox 1.3
#define oz 1.5

float smin(float a,float b,float k){float h=clamp(0.5+0.5*(b-a)/k,0.0,1.0);return mix(b,a,h)-k*h*(1.0-h);}
float smax(float a,float b,float k){return smin(a,b,-k);}


vec3 bendCathedral(vec3 p){
    float arch = sin(length(p.xz)*0.5 + iTime*0.2)*1.5;
    p.y += arch - 0.3*length(p.xz);
    p.x += 0.2*sin(p.z*0.5 + iTime*0.5);
    p.z += 0.2*sin(p.x*0.5 + iTime*0.4);
    return p;
}

float map(vec3 p){
    p = bendCathedral(p);
    

    float ground = p.y + 0.25;
    

    vec3 q0=p; q0.x=mod(q0.x-ox,ox+ox)-ox; q0.z=mod(q0.z-oz*0.5,oz)-oz*0.5;
    vec3 q1=p; q1.x=mod(q1.x,ox+ox)-ox; q1.z=mod(q1.z,oz)-oz*0.5;
    
    float hex0 = sdHexPrism(q0.xzy, vec2(0.25+0.3,1.5))-0.02;
    float hex1 = sdHexPrism(q1.xzy, vec2(0.25+0.3,1.5))-0.02;
    
    float res = smin(hex0,hex1,0.08);
    return smin(res,ground,0.4);
}

float mat(vec3 p){
    return (map(p) < 0.001) ? 1.0 : 0.0;
}

vec3 getNormal(vec3 p){
    const vec3 e=vec3(0.07,0,0);
    return normalize(vec3(map(p+e)-map(p-e),map(p+e.yxz)-map(p-e.yxz),map(p+e.zyx)-map(p-e.zyx)));
}

float getShadow(vec3 ro,vec3 rd,float minD,float maxD,float k){
    float res=1.0,d=minD;
    for(int i=0;i<40;i++){
        float s=map(ro+rd*d);
        if(abs(s)<1e-5) return 0.0;
        res=min(res,k*s/d);
        d+=max(0.06,s);
        if(d>=maxD) break;
    }
    return clamp(res,0.0,1.0);
}

vec3 cam(vec2 uv,vec3 ro,vec3 cv,float fov){
    vec3 cu=normalize(vec3(0,1,0));
    vec3 z=normalize(cv-ro);
    vec3 x=normalize(cross(cu,z));
    vec3 y=cross(z,x);
    return normalize(z + fov*uv.x*x + fov*uv.y*y);
}

vec3 hsv2rgb_smooth(vec3 c){
    vec3 rgb=clamp(abs(mod(c.x*6.0+vec3(0,4,2),6.0)-3.0)-1.0,0.0,1.0);
    rgb=rgb*rgb*(3.0-2.0*rgb);
    return c.z*mix(vec3(1.0),rgb,c.y);
}

void mainImage(out vec4 fragColor,in vec2 fragCoord){
    vec2 si=iResolution.xy;
    vec2 uvc=(2.*fragCoord.xy-si)/si.y;

    float tm = iTime*0.25;

    vec3 ro = vec3(sin(tm)*9.5, 15.0 + 0.8*sin(iTime*0.7), cos(tm)*9.5);
    vec3 cv = vec3(0.0, 1.0, 0.0);
    vec3 rd = cam(uvc, ro, cv, 0.6);

    vec3 col=vec3(0);
    vec3 p=ro;
    float s=1.0,d=0.0;
    const float md=80.0;

    for(int i=0;i<200;i++){
        if(d>md) break;
        s=map(p);
        d+=s*0.6;
        if(abs(s)<0.001) break;
        p=ro+rd*d;
    }

    if(d < md){
        vec3 n = getNormal(p);
        vec3 lp = normalize(vec3(4.0,8.0,2.0));
        float diff = max(dot(n,lp),0.0);
        float sha = getShadow(p + 0.02*n, lp, 0.02, 100.0, 4.0);
        float spe = pow(max(dot(-rd, reflect(-lp,n)),0.0),36.0);

        float m = mat(p);

        float fres = pow(1.0 - max(dot(rd,n),0.0), 3.0);

        if(m > 0.5){
            vec3 capBase = mix(vec3(0.6,0.85,1.0), vec3(0.3,0.95,1.2), 0.5 + 0.5*sin(iTime*1.5 + length(p.xz)*0.8));
            vec3 chromatic = vec3(sin(iTime*2.0 + p.x*2.0), sin(iTime*1.7 + p.y*3.0), sin(iTime*1.3 + p.z*2.5)) * 0.15;
            vec3 cap = capBase + chromatic;
            cap = clamp(cap,0.0,1.0);
            col = cap * (0.6*diff + 0.35*sha) + 0.6*spe + fres*0.25; 
        } else {
            float hue = atan(p.x,p.z)/6.2831 + iTime*2.0;
            vec3 base = hsv2rgb_smooth(vec3(hue,0.95,1.0));
            vec3 edgeColor = vec3(sin(hue*6.28 + iTime*2.0), cos(hue*6.28 + iTime*2.2), sin(hue*6.28 + iTime*1.8)) * 0.25;
            base = clamp(base + edgeColor, 0.0, 1.0);
            col = base*(0.5*diff + 0.5*sha) + 0.2*spe + fres*0.3;
        }

        col *= 0.9 + 0.1*diff;
    } else {
        float t = 0.5 + 0.5*rd.y;
        col = mix(vec3(0.03,0.02,0.06), vec3(0.1,0.15,0.28), pow(max(t,0.0),1.5));
    }

    float fog=exp(-0.005*d*d);
    col=mix(vec3(0.02,0.02,0.03),col,fog);

    fragColor=vec4(clamp(col,0.0,1.0),1.0);
}
