Shader "Custom/DebugUVs" {
	Properties {
		_Amnt ("Amount", float) = 1.0
	}

	SubShader {
		Pass {
			CGPROGRAM
			#include "UnityCG.cginc"
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			float4 _LightColor0;
			float _Amnt;

			struct vsIn {
				float4 position : POSITION;
				float3 normal : NORMAL;
				float2 uv : TEXCOORD0;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				float3 normal : NORMAL;
				float4 uv : TEXCOORD0;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.position = UnityObjectToClipPos(v.position);
				o.uv = float4 ( v.uv.xy, 0, 0);
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				half4 c = frac( psIn.uv );
				if (any(saturate(psIn.uv) - psIn.uv)){
					c.b = 0.5;
				}
				return c;
			}
			ENDCG
		}
	}
}
