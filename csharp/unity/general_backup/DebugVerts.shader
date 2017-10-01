Shader "Custom/DebugVerts" {
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
				float4 color : COLOR;
				float2 uv : TEXCOORD0;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				float4 color : COLOR;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.position = UnityObjectToClipPos(v.position);
				o.color = v.color;
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				return psIn.color;
			}
			ENDCG
		}
	}
}
