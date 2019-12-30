// Upgrade NOTE: replaced '_World2Object' with 'unity_WorldToObject'

Shader "Custom/TextureSimple" {
	Properties {
		_MainTexture ("Main Texture", 2D) = "white" {}
		_TexChange ("Tex Change", float) = 0
	}

	SubShader {
		Tags { "LightMode" = "ForwardBase" }
		Pass {
			CGPROGRAM
			#include "UnityCG.cginc"
			#pragma target 2.0
			#pragma vertex vert
			#pragma fragment frag

			sampler2D _MainTexture;
			float4 _LightColor0;
			float _TexChange;

			struct vsIn {
				float4 position : POSITION;
				float3 normal : NORMAL;
				float2 uv : TEXCOORD0;
			};

			struct vsOut {
				float4 position : SV_POSITION;
				float3 normal : NORMAL;
				float2 uv : TEXCOORD0;
			};

			vsOut vert(vsIn v){
				vsOut o;
				o.position = UnityObjectToClipPos(v.position);
				o.normal = normalize(mul(float4(v.normal, 0.0), unity_WorldToObject));
				o.uv = v.uv;
				return o;
			}

			float4 frag(vsOut psIn) : SV_TARGET {
				float4 ambientLight = UNITY_LIGHTMODEL_AMBIENT;

				float4 LightDirection = normalize(_WorldSpaceLightPos0);
				float4 diffuseTerm = saturate(dot(LightDirection, psIn.normal));
				float4 diffuseLight = diffuseTerm * _LightColor0;

				float csin = _SinTime[0] + _SinTime[1] + _SinTime[2];

				//float4 tex = tex2D(_MainTexture, sin(20*(psIn.uv + csin)));
				float4 tex = tex2D(_MainTexture, psIn.uv);
				return ambientLight + diffuseLight + tex;
			}
			ENDCG
		}
	}
}
